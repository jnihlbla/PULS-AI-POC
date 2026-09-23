000100***************************************************************           
000200 ID  DIVISION.                                                            
000300***************************************************************           
000400 PROGRAM-ID.    V1616040.                                                 
000500 AUTHOR.        CARINA HOLMQVIST                                          
000600 DATE-WRITTEN.  SEPTEMBER 1991                                            
000700***************************************************************           
000800*                                                                         
000900*REMARKS:                                                                 
001000*                                                                         
001100*  * ACTION CODE V (VIEW) ON EXTRACTADM PANEL                             
001200*                                                                         
001300*  * DB2-TABLES:                                                          
001400*                                                                         
001500*        EXTRACT                                                          
001600*                                                                         
001700*  * SUBPROGRAMS:                                                         
001800*                                                                         
001900*        V1619600, V1619700                                               
002000*                                                                         
002100*  * PARMS:                                                               
002200*                                                                         
002300*        EXTRACTID X(8)                                                   
002400*                                                                         
002500*  * RETURNCODES:                                                         
002600*                                                                         
002700*         4 - FUNCTION VIEW CANCELLED                                     
002800*         8 - EXTRACTID NOT FOUND                                         
002900*        12 - INVALID PARM                                                
003000*        16 - SEVERE ISPF-ERROR                                           
003100*        20 - SEVERE DB2-ERROR                                            
003200*                                                                         
003300***************************************************************           
003400     EJECT                                                                
003500***************************************************************           
003600 ENVIRONMENT DIVISION.                                                    
003700***************************************************************           
003800     SKIP2                                                                
003900*--------------------------------------------------------------           
004000 CONFIGURATION SECTION.                                                   
004100*--------------------------------------------------------------           
004200 SOURCE-COMPUTER. IBM-370.                                                
004300*SOURCE-COMPUTER. IBM-370 WITH DEBUGGING MODE.                            
004400     SKIP2                                                                
004500***************************************************************           
004600 DATA DIVISION.                                                           
004700***************************************************************           
004800     SKIP2                                                                
004900*--------------------------------------------------------------           
005000 WORKING-STORAGE SECTION.                                                 
005100*--------------------------------------------------------------           
005200 01  PROGRAM-NAME                 PIC X(8)  VALUE 'V1616040'.             
005300 01  ABEND-SECTION                PIC X(25) VALUE SPACE.                  
005400 01  RETURN-CODES.                                                        
005500     03  RCODE                    PIC S9(4) COMP SYNC VALUE ZERO.         
005600     03  RCODE-4                  PIC S9(4) COMP SYNC VALUE 4.            
005700     03  RCODE-8                  PIC S9(4) COMP SYNC VALUE 8.            
005800     03  RCODE-12                 PIC S9(4) COMP SYNC VALUE 12.           
005900     03  RCODE-16                 PIC S9(4) COMP SYNC VALUE 16.           
006000     03  RCODE-20                 PIC S9(4) COMP SYNC VALUE 20.           
006100     03  RCODE-DISPL              PIC Z(4)-.                              
006200     SKIP2                                                                
006300 01  GENERAL-CONSTANTS.                                                   
006400     03 YES                       PIC X(1)  VALUE 'Y'.                    
006500     03 NOO                       PIC X(1)  VALUE 'N'.                    
006600     SKIP2                                                                
006700 01  SWITCHES.                                                            
006800     03 SW-ENTER-PRESSED          PIC X(1)  VALUE 'N'.                    
006900     03 SW-ERROR                  PIC X(1)  VALUE 'N'.                    
007000     SKIP2                                                                
007100 01  W-AREAS.                                                             
007200     03 W-ERROR-MESSAGE           PIC X(40) VALUE SPACE.                  
007300     03 W-SIZE                    PIC ZZZZZ.                              
007400     03 W-VALID-NO-DAYS           PIC ZZZ.                                
007500     03 W-NULL-INDIKATOR1         PIC S9(4) COMP.                         
007600     SKIP2                                                                
007700 01  N-PANEL-V161604M.                                                    
007800     03 N-EXTRACTID               PIC X(8)  VALUE 'V161604A'.             
007900     03 N-TYPE                    PIC X(8)  VALUE 'V161604B'.             
008000     03 N-COPYTEXT                PIC X(8)  VALUE 'V161604C'.             
008100     03 N-DESCRIPTION             PIC X(8)  VALUE 'V161604D'.             
008200     03 N-SIZE                    PIC X(8)  VALUE 'V161604E'.             
008300     03 N-VALID-NO-DAYS           PIC X(8)  VALUE 'V161604F'.             
008400     03 N-LAST-UPDATED            PIC X(8)  VALUE 'V161604G'.             
008500     03 N-OWNER                   PIC X(8)  VALUE 'V161604H'.             
008600     03 N-NAME                    PIC X(8)  VALUE 'V161604I'.             
008700     03 N-EXTRACT-FILE            PIC X(8)  VALUE 'V161604J'.             
008800     03 N-EXTRACT-JOB             PIC X(8)  VALUE 'V161604K'.             
008900     03 N-JOB-TYPE                PIC X(8)  VALUE 'V161604L'.             
009000     SKIP2                                                                
009100 01  LTH-PANEL-V161604M.                                                  
009200     03 LTH-EXTRACTID             PIC S9(6) VALUE 8  COMP.                
009300     03 LTH-TYPE                  PIC S9(6) VALUE 8  COMP.                
009400     03 LTH-COPYTEXT              PIC S9(6) VALUE 8  COMP.                
009500     03 LTH-DESCRIPTION           PIC S9(6) VALUE 25 COMP.                
009600     03 LTH-SIZE                  PIC S9(6) VALUE 5  COMP.                
009700     03 LTH-VALID-NO-DAYS         PIC S9(6) VALUE 3  COMP.                
009800     03 LTH-LAST-UPDATED          PIC S9(6) VALUE 10 COMP.                
009900     03 LTH-OWNER                 PIC S9(6) VALUE 7  COMP.                
010000     03 LTH-NAME                  PIC S9(6) VALUE 20 COMP.                
010100     03 LTH-EXTRACT-FILE          PIC S9(6) VALUE 42 COMP.                
010200     03 LTH-EXTRACT-JOB           PIC S9(6) VALUE 52 COMP.                
010300     03 LTH-JOB-TYPE              PIC S9(6) VALUE 1  COMP.                
010400     SKIP2                                                                
010500 01  ISPF-PANEL-V161604M.                                                 
010600     03 ISPF-EXTRACTID            PIC X(8)  VALUE SPACE.                  
010700     03 ISPF-TYPE                 PIC X(8)  VALUE SPACE.                  
010800     03 ISPF-COPYTEXT             PIC X(8)  VALUE SPACE.                  
010900     03 ISPF-DESCRIPTION          PIC X(25) VALUE SPACE.                  
011000     03 ISPF-SIZE                 PIC X(5)  VALUE SPACE.                  
011100     03 ISPF-VALID-NO-DAYS        PIC X(3)  VALUE SPACE.                  
011200     03 ISPF-LAST-UPDATED         PIC X(10) VALUE SPACE.                  
011300     03 ISPF-OWNER                PIC X(7)  VALUE SPACE.                  
011400     03 ISPF-NAME                 PIC X(20) VALUE SPACE.                  
011500     03 ISPF-EXTRACT-FILE         PIC X(42) VALUE SPACE.                  
011600     03 ISPF-EXTRACT-JOB          PIC X(52) VALUE SPACE.                  
011700     03 ISPF-JOB-TYPE             PIC X(1)  VALUE SPACE.                  
011800     SKIP2                                                                
011900*--------------------------------------------------------------           
012000*DEKLARATION AV ISPF-CONSTANTS                                            
012100*--------------------------------------------------------------           
012200 01  DISPLAYE                     PIC X(8)  VALUE 'DISPLAY '.             
012300 01  VDEFINE                      PIC X(8)  VALUE 'VDEFINE '.             
012400 01  VDELETE                      PIC X(8)  VALUE 'VDELETE '.             
012500 01  CHAR                         PIC X(8)  VALUE 'CHAR    '.             
012600*--------------------------------------------------------------           
012700*DEKLARATION AV ISPF-FÄLT                                                 
012800*--------------------------------------------------------------           
012900 01  V161604M                     PIC X(8)  VALUE 'V1616040'.             
013000 01  ISPF-CURSOR                  PIC X(8)  VALUE SPACE.                  
013100 01  ISPF-MSG-ID                  PIC X(8)  VALUE SPACE.                  
013200 01  DYNAMIC-SUBPROGRAMS.                                                 
013300     03 V16196                    PIC X(8)  VALUE 'V16196  '.             
013400     03 V16197                    PIC X(8)  VALUE 'V16197  '.             
013500     SKIP2                                                                
013600 01  V1619600-PARMS.                                                      
013700     03 V1619600-DSNAME           PIC X(44) VALUE SPACE.                  
013800     03 V1619600-VALID-NO-DAYS    PIC 999   VALUE ZERO.                   
013900     03 V1619600-LAST-UPDATED     PIC X(6)  VALUE SPACE.                  
014000     03 V1619600-VALID-TO-DATE    PIC X(6)  VALUE SPACE.                  
014100 01  V1619700-PARMS.                                                      
014200     03 V1619700-USERID           PIC X(7)  VALUE SPACE.                  
014300     03 V1619700-NAME             PIC X(20) VALUE ZERO.                   
014400     EJECT                                                                
014500*-------------------------------- DB2 ERROR HANDLING                      
014600*    -COPY V161WS                                                         
014700*++INCLUDE V161WS                                                         
014800*-------------------------------- DB2-AREAS                               
014900     EXEC SQL                                                             
015000          INCLUDE SQLCA                                                   
015100     END-EXEC.                                                            
015200     EXEC SQL                                                             
015300          INCLUDE EXTRACT                                                 
015400     END-EXEC.                                                            
015500*    -COPY EXTRACT -PRE EXTRACT-                                          
015600     EJECT                                                                
015700*--------------------------------------------------------------           
015800 LINKAGE SECTION.                                                         
015900*--------------------------------------------------------------           
016000 01 PARM.                                                                 
016100     03 PARM-EXTRACTID         PIC X(8).                                  
016200*************************************************************             
016300 PROCEDURE DIVISION USING PARM.                                           
016400*************************************************************             
016500     PERFORM A-INIT                                                       
016600     IF SW-ERROR = NOO                                                    
016700       PERFORM B-SELECT-PANELINFO                                         
016800       IF SW-ERROR = NOO                                                  
016900          PERFORM C-DISPLAY-PANEL                                         
017000          PERFORM UNTIL SW-ENTER-PRESSED = NOO                            
017100            PERFORM C-DISPLAY-PANEL                                       
017200          END-PERFORM                                                     
017300       END-IF                                                             
017400     END-IF                                                               
017500     PERFORM Z-FINIT                                                      
017600     GOBACK                                                               
017700     CONTINUE.                                                            
017800                                                                          
017900*--------------------------------------------------------------           
018000 A-INIT SECTION.                                                          
018100*--------------------------------------------------------------           
018200     MOVE 'A-INIT                  ' TO ABEND-SECTION                     
018300D    DISPLAY ABEND-SECTION                                                
018400     SKIP2                                                                
018500     MOVE NOO TO SW-ERROR                                                 
018600     PERFORM AA-INIT-PARMS                                                
018700     PERFORM AB-VDEF-PANEL-V161604M                                       
018800     CONTINUE.                                                            
018900     EJECT                                                                
019000*--------------------------------------------------------------           
019100 AA-INIT-PARMS    SECTION.                                                
019200*--------------------------------------------------------------           
019300     MOVE 'AA-INIT-PARMS           ' TO ABEND-SECTION                     
019400D    DISPLAY ABEND-SECTION                                                
019500     SKIP2                                                                
019600D    DISPLAY 'PARM-EXTRACTID    :' PARM-EXTRACTID                         
019700     IF PARM-EXTRACTID = SPACE                                            
019800        MOVE RCODE-12 TO RCODE                                            
019900        MOVE YES TO SW-ERROR                                              
020000     END-IF                                                               
020100     CONTINUE.                                                            
020200     EJECT                                                                
020300*--------------------------------------------------------------           
020400 AB-VDEF-PANEL-V161604M SECTION.                                          
020500*--------------------------------------------------------------           
020600     MOVE 'AB-VDEF-PANEL-V161604M    ' TO ABEND-SECTION                   
020700D    DISPLAY ABEND-SECTION                                                
020800     SKIP2                                                                
020900     CALL 'ISPLINK' USING VDEFINE N-EXTRACTID ISPF-EXTRACTID              
021000                          CHAR LTH-EXTRACTID                              
021100     CALL 'ISPLINK' USING VDEFINE N-TYPE      ISPF-TYPE                   
021200                          CHAR LTH-TYPE                                   
021300     CALL 'ISPLINK' USING VDEFINE N-COPYTEXT  ISPF-COPYTEXT               
021400                          CHAR LTH-COPYTEXT                               
021500     CALL 'ISPLINK' USING VDEFINE N-DESCRIPTION ISPF-DESCRIPTION          
021600                          CHAR LTH-DESCRIPTION                            
021700     CALL 'ISPLINK' USING VDEFINE N-SIZE ISPF-SIZE                        
021800                          CHAR  LTH-SIZE                                  
021900     CALL 'ISPLINK' USING VDEFINE N-VALID-NO-DAYS                         
022000                                  ISPF-VALID-NO-DAYS                      
022100                                  CHAR                                    
022200                                  LTH-VALID-NO-DAYS                       
022300     CALL 'ISPLINK' USING VDEFINE N-LAST-UPDATED ISPF-LAST-UPDATED        
022400                          CHAR  LTH-LAST-UPDATED                          
022500     CALL 'ISPLINK' USING VDEFINE N-OWNER   ISPF-OWNER                    
022600                          CHAR  LTH-OWNER                                 
022700     CALL 'ISPLINK' USING VDEFINE N-NAME         ISPF-NAME                
022800                          CHAR  LTH-NAME                                  
022900     CALL 'ISPLINK' USING VDEFINE N-EXTRACT-FILE ISPF-EXTRACT-FILE        
023000                          CHAR  LTH-EXTRACT-FILE                          
023100     CALL 'ISPLINK' USING VDEFINE N-EXTRACT-JOB ISPF-EXTRACT-JOB          
023200                          CHAR  LTH-EXTRACT-JOB                           
023300     CALL 'ISPLINK' USING VDEFINE N-JOB-TYPE    ISPF-JOB-TYPE             
023400                          CHAR  LTH-JOB-TYPE                              
023500                                                                          
023600     CONTINUE.                                                            
023700     EJECT                                                                
023800*--------------------------------------------------------------           
023900 B-SELECT-PANELINFO SECTION.                                              
024000*--------------------------------------------------------------           
024100     MOVE 'B-SELECT-PANELINFO      ' TO ABEND-SECTION                     
024200D    DISPLAY ABEND-SECTION                                                
024300     SKIP2                                                                
024400     MOVE PARM-EXTRACTID     TO EXTRACT-EXTRACTID                         
024500     PERFORM SQL-SELECT-EXTRACTID                                         
024600     IF SQLCODE = +100                                                    
024700        MOVE RCODE-8 TO RCODE                                             
024800        MOVE YES TO SW-ERROR                                              
024900     END-IF                                                               
025000     MOVE EXTRACT-OWNER   TO V1619700-USERID                              
025100     CALL V16197 USING V1619700-USERID                                    
025200                       V1619700-NAME                                      
025300     IF RETURN-CODE = 0 THEN                                              
025400        MOVE V1619700-NAME    TO ISPF-NAME                                
025500     ELSE                                                                 
025600        MOVE 'NAME NOT FOUND' TO ISPF-NAME                                
025700     END-IF                                                               
025800     MOVE EXTRACT-EXTRACTID    TO ISPF-EXTRACTID                          
025900     MOVE EXTRACT-TYPEX        TO ISPF-TYPE                               
026000     MOVE EXTRACT-COPYTEXT     TO ISPF-COPYTEXT                           
026100     MOVE EXTRACT-DESCRIPTION  TO ISPF-DESCRIPTION                        
026200     MOVE EXTRACT-SIZEX        TO W-SIZE                                  
026300     MOVE W-SIZE               TO ISPF-SIZE                               
026400     MOVE EXTRACT-OWNER   TO ISPF-OWNER                                   
026500     MOVE EXTRACT-EXTRACT-FILE      TO V1619600-DSNAME,                   
026600                                       ISPF-EXTRACT-FILE                  
026700     MOVE EXTRACT-VALID-NO-DAYS     TO V1619600-VALID-NO-DAYS             
026800     MOVE SPACE                     TO V1619600-LAST-UPDATED              
026900     MOVE SPACE                     TO V1619600-VALID-TO-DATE             
027000     CALL V16196 USING V1619600-DSNAME                                    
027100                       V1619600-VALID-NO-DAYS                             
027200                       V1619600-LAST-UPDATED                              
027300                       V1619600-VALID-TO-DATE                             
027400     IF RETURN-CODE = 0 THEN                                              
027500        MOVE V1619600-LAST-UPDATED TO ISPF-LAST-UPDATED                   
027600     END-IF                                                               
027700     MOVE EXTRACT-VALID-NO-DAYS TO W-VALID-NO-DAYS                        
027800     MOVE W-VALID-NO-DAYS       TO ISPF-VALID-NO-DAYS                     
027900     MOVE EXTRACT-EXTRACT-JOB   TO ISPF-EXTRACT-JOB                       
028000     MOVE EXTRACT-JOB-TYPE      TO ISPF-JOB-TYPE                          
028100     CONTINUE.                                                            
028200     EJECT                                                                
028300*--------------------------------------------------------------           
028400 C-DISPLAY-PANEL SECTION.                                                 
028500*--------------------------------------------------------------           
028600     MOVE 'C-DISPLAY-PANEL         ' TO ABEND-SECTION                     
028700D    DISPLAY ABEND-SECTION                                                
028800     SKIP2                                                                
028900     PERFORM S01-DISPLAY-PANEL                                            
029000     IF RCODE = 0                                                         
029100        MOVE YES TO SW-ENTER-PRESSED                                      
029200     ELSE                                                                 
029300        MOVE NOO TO SW-ENTER-PRESSED                                      
029400        IF RCODE = 8                                                      
029500          MOVE RCODE-4 TO RCODE                                           
029600        END-IF                                                            
029700     END-IF                                                               
029800     CONTINUE.                                                            
029900     EJECT                                                                
030000*--------------------------------------------------------------           
030100 Z-FINIT SECTION.                                                         
030200*--------------------------------------------------------------           
030300     MOVE 'Z-FINIT                  ' TO ABEND-SECTION                    
030400D    DISPLAY ABEND-SECTION                                                
030500     SKIP2                                                                
030600     PERFORM ZA-VDEL-PANEL-V161604M                                       
030700     MOVE RCODE TO RETURN-CODE                                            
030800     CONTINUE.                                                            
030900     EJECT                                                                
031000*--------------------------------------------------------------           
031100 ZA-VDEL-PANEL-V161604M SECTION.                                          
031200*--------------------------------------------------------------           
031300     MOVE 'ZA-VDEL-PANEL-V161604M    ' TO ABEND-SECTION                   
031400D    DISPLAY ABEND-SECTION                                                
031500     SKIP2                                                                
031600     CALL 'ISPLINK' USING VDELETE N-EXTRACTID                             
031700     CALL 'ISPLINK' USING VDELETE N-TYPE                                  
031800     CALL 'ISPLINK' USING VDELETE N-COPYTEXT                              
031900     CALL 'ISPLINK' USING VDELETE N-DESCRIPTION                           
032000     CALL 'ISPLINK' USING VDELETE N-SIZE                                  
032100     CALL 'ISPLINK' USING VDELETE N-VALID-NO-DAYS                         
032200     CALL 'ISPLINK' USING VDELETE N-LAST-UPDATED                          
032300     CALL 'ISPLINK' USING VDELETE N-OWNER                                 
032400     CALL 'ISPLINK' USING VDELETE N-NAME                                  
032500     CALL 'ISPLINK' USING VDELETE N-EXTRACT-FILE                          
032600     CALL 'ISPLINK' USING VDELETE N-EXTRACT-JOB                           
032700     CALL 'ISPLINK' USING VDELETE N-JOB-TYPE                              
032800     CONTINUE.                                                            
032900     EJECT                                                                
033000*--------------------------------------------------------------           
033100 S01-DISPLAY-PANEL SECTION.                                               
033200*--------------------------------------------------------------           
033300     MOVE 'S01-DISPLAY-PANEL           ' TO ABEND-SECTION                 
033400D    DISPLAY ABEND-SECTION                                                
033500     SKIP2                                                                
033600     CALL 'ISPLINK' USING DISPLAYE V161604M                               
033700                          ISPF-MSG-ID ISPF-CURSOR                         
033800     MOVE RETURN-CODE TO RCODE RCODE-DISPL                                
033900     IF RCODE > +12                                                       
034000        PERFORM S99-ERROR-ROUTINE                                         
034100        MOVE YES TO SW-ERROR                                              
034200        MOVE RCODE-16 TO RCODE                                            
034300     END-IF                                                               
034400     CONTINUE.                                                            
034500     EJECT                                                                
034600*--------------------------------------------------------------           
034700 S99-ERROR-ROUTINE SECTION.                                               
034800*--------------------------------------------------------------           
034900     SKIP2                                                                
035000     STRING 'ERROR IN ' ABEND-SECTION ' RCODE ='                          
035100     RCODE-DISPL DELIMITED BY SIZE                                        
035200     INTO W-ERROR-MESSAGE                                                 
035300     DISPLAY W-ERROR-MESSAGE                                              
035400     CONTINUE.                                                            
035500     EJECT                                                                
035600*--------------------------------------------------------------           
035700 SQL-SELECT-EXTRACTID SECTION.                                            
035800*--------------------------------------------------------------           
035900     MOVE 'SQL-SELECT-EXTRACTID        ' TO ABEND-SECTION                 
036000D    DISPLAY ABEND-SECTION                                                
036100     SKIP2                                                                
036200     EXEC SQL                                                             
036300        SELECT  EXTRACTID, TYPE, COPYTEXT, DESCRIPTION, SIZE,             
036400                VALID_NO_DAYS, EXTRACT_FILE,                              
036500                EXTRACT_JOB, JOB_TYPE, OWNER                              
036600        INTO :EXTRACT-EXTRACTID,                                          
036700             :EXTRACT-TYPEX,                                              
036800             :EXTRACT-COPYTEXT:W-NULL-INDIKATOR1,                         
036900             :EXTRACT-DESCRIPTION,                                        
037000             :EXTRACT-SIZEX,                                              
037100             :EXTRACT-VALID-NO-DAYS,                                      
037200             :EXTRACT-EXTRACT-FILE,                                       
037300             :EXTRACT-EXTRACT-JOB,                                        
037400             :EXTRACT-JOB-TYPE,                                           
037500             :EXTRACT-OWNER                                               
037600        FROM EXTRACT                                                      
037700        WHERE EXTRACTID = :EXTRACT-EXTRACTID                              
037800     END-EXEC                                                             
037900     IF SQLCODE = +000 AND W-NULL-INDIKATOR1 < 0                          
038000        MOVE SPACE TO EXTRACT-COPYTEXT                                    
038100     END-IF                                                               
038200     PERFORM S95-CONTROL-SQLCODE                                          
038300     CONTINUE.                                                            
038400     EJECT                                                                
038500*-------------------------------- DB2 ERROR HANDLING                      
038600*    -COPY V161PS                                                         
038700*++INCLUDE V161PS                                                         
