000100***************************************************************           
000200 ID  DIVISION.                                                            
000300***************************************************************           
000400 PROGRAM-ID.    V1616030.                                                 
000500 AUTHOR.        CARINA HOLMQVIST                                          
000600 DATE-WRITTEN.  SEPTEMBER 1991                                            
000700***************************************************************           
000800*                                                                         
000900*REMARKS:                                                                 
001000*                                                                         
001100*  * ACTION CODE U (UPDATE) ON EXTRACT ADM PANEL                          
001200*                                                                         
001300*  * DB2-TABLES:                                                          
001400*                                                                         
001500*        EXTRACT                                                          
001600*                                                                         
001700*  * SUBPROGRAMS                                                          
001800*                                                                         
001900*        V16194 V16195 V16198                                             
002000*                                                                         
002100*  * PARMS:                                                               
002200*                                                                         
002300*        EXTRACTID X(8)                                                   
002400*                                                                         
002500*                                                                         
002600*  * RETURNCODES:                                                         
002700*                                                                         
002800*         4 - FUNCTION UPDATE CANCELLED                                   
002900*         8 - NO AUTHORIZATION                                            
003000*        12 - INVALID PARM                                                
003100*        16 - SEVERE ISPF-ERROR                                           
003200*        20 - SEVERE DB2-ERROR                                            
003300*                                                                         
003400***************************************************************           
003500     EJECT                                                                
003600***************************************************************           
003700 ENVIRONMENT DIVISION.                                                    
003800***************************************************************           
003900     SKIP2                                                                
004000*--------------------------------------------------------------           
004100 CONFIGURATION SECTION.                                                   
004200*--------------------------------------------------------------           
004300*SOURCE-COMPUTER. IBM-370.                                                
004400 SOURCE-COMPUTER. IBM-370 WITH DEBUGGING MODE.                            
004500     SKIP2                                                                
004600***************************************************************           
004700 DATA DIVISION.                                                           
004800***************************************************************           
004900     SKIP2                                                                
005000*--------------------------------------------------------------           
005100 WORKING-STORAGE SECTION.                                                 
005200*--------------------------------------------------------------           
005300 01  PROGRAM-NAME                 PIC X(8)  VALUE 'V1616030'.             
005400 01  ABEND-SECTION                PIC X(25) VALUE SPACE.                  
005500 01  RETURN-CODES.                                                        
005600     03  RCODE                    PIC S9(4) COMP SYNC VALUE ZERO.         
005700     03  RCODE-4                  PIC S9(4) COMP SYNC VALUE 4.            
005800     03  RCODE-8                  PIC S9(4) COMP SYNC VALUE 8.            
005900     03  RCODE-12                 PIC S9(4) COMP SYNC VALUE 12.           
006000     03  RCODE-16                 PIC S9(4) COMP SYNC VALUE 16.           
006100     03  RCODE-20                 PIC S9(4) COMP SYNC VALUE 20.           
006200     03  RCODE-DISPL              PIC Z(4)-.                              
006300     SKIP2                                                                
006400 01  GENERAL-CONSTANTS.                                                   
006500     03 YES                       PIC X(1)  VALUE 'Y'.                    
006600     03 NOO                       PIC X(1)  VALUE 'N'.                    
006700     SKIP2                                                                
006800 01  SWITCHES.                                                            
006900     03 SW-ENTER-PRESSED          PIC X(1)  VALUE 'N'.                    
007000     03 SW-ERROR                  PIC X(1)  VALUE 'N'.                    
007100     SKIP2                                                                
007200 01  W-AREAS.                                                             
007300     03 W-EXTRACTID               PIC X(8)  VALUE SPACE.                  
007400     03 W-ERROR-MESSAGE           PIC X(40) VALUE SPACE.                  
007500     03 W-JUST-TAB1.                                                      
007600        05 W-JUST-1               PIC X(1) OCCURS 5                       
007700                                  INDEXED BY IX-TAB1.                     
007800     03 W-JUST-TAB2.                                                      
007900        05 W-JUST-2               PIC X(1) OCCURS 5                       
008000                                  INDEXED BY IX-TAB2.                     
008100     03 W-NULL-INDIKATOR1         PIC S9(4) COMP.                         
008200     SKIP2                                                                
008300 01  N-PANEL-V161603M.                                                    
008400     03 N-EXTRACTID               PIC X(8)  VALUE 'V161603A'.             
008500     03 N-TYPE                    PIC X(8)  VALUE 'V161603B'.             
008600     03 N-COPYTEXT                PIC X(8)  VALUE 'V161603C'.             
008700     03 N-DESCRIPTION             PIC X(8)  VALUE 'V161603D'.             
008800     03 N-SIZE                    PIC X(8)  VALUE 'V161603E'.             
008900     03 N-VALID-NO-DAYS           PIC X(8)  VALUE 'V161603F'.             
009000     03 N-EXTRACT-FILE            PIC X(8)  VALUE 'V161603G'.             
009100     03 N-EXTRACT-JOB             PIC X(8)  VALUE 'V161603H'.             
009200     03 N-JOB-TYPE                PIC X(8)  VALUE 'V161603I'.             
009300     SKIP2                                                                
009400 01  LTH-PANEL-V161603M.                                                  
009500     03 LTH-EXTRACTID             PIC S9(6) VALUE 8  COMP.                
009600     03 LTH-TYPE                  PIC S9(6) VALUE 8  COMP.                
009700     03 LTH-COPYTEXT              PIC S9(6) VALUE 8  COMP.                
009800     03 LTH-DESCRIPTION           PIC S9(6) VALUE 25 COMP.                
009900     03 LTH-SIZE                  PIC S9(6) VALUE 5  COMP.                
010000     03 LTH-VALID-NO-DAYS         PIC S9(6) VALUE 3  COMP.                
010100     03 LTH-EXTRACT-FILE          PIC S9(6) VALUE 42 COMP.                
010200     03 LTH-EXTRACT-JOB           PIC S9(6) VALUE 52 COMP.                
010300     03 LTH-JOB-TYPE              PIC S9(6) VALUE 1  COMP.                
010400     SKIP2                                                                
010500 01  ISPF-PANEL-V161603M.                                                 
010600     03 ISPF-EXTRACTID            PIC X(8)  VALUE SPACE.                  
010700     03 ISPF-TYPE                 PIC X(8)  VALUE SPACE.                  
010800     03 ISPF-COPYTEXT             PIC X(8)  VALUE SPACE.                  
010900     03 ISPF-DESCRIPTION          PIC X(25) VALUE SPACE.                  
011000     03 ISPF-SIZE                 PIC X(5)  VALUE SPACE.                  
011100     03 ISPF-VALID-NO-DAYS        PIC X(3)  VALUE SPACE.                  
011200     03 ISPF-EXTRACT-FILE         PIC X(42) VALUE SPACE.                  
011300     03 ISPF-EXTRACT-JOB          PIC X(52) VALUE SPACE.                  
011400     03 ISPF-JOB-TYPE             PIC X(1)  VALUE SPACE.                  
011500     SKIP2                                                                
011600 01  W-SIZE                       PIC ZZZZZ.                              
011700 01  W-VALID-NO-DAYS              PIC ZZZ.                                
011800*--------------------------------------------------------------           
011900*ISPF-CONSTANTS                                                           
012000*--------------------------------------------------------------           
012100 01  DISPLAYE                     PIC X(8)  VALUE 'DISPLAY '.             
012200 01  VDEFINE                      PIC X(8)  VALUE 'VDEFINE '.             
012300 01  VDELETE                      PIC X(8)  VALUE 'VDELETE '.             
012400 01  CHAR                         PIC X(8)  VALUE 'CHAR    '.             
012500*--------------------------------------------------------------           
012600*ISPF-FIELDS                                                              
012700*--------------------------------------------------------------           
012800 01  V161603M                     PIC X(8)  VALUE 'V1616030'.             
012900 01  ISPF-CURSOR                  PIC X(8)  VALUE SPACE.                  
013000 01  ISPF-MSG-ID                  PIC X(8)  VALUE SPACE.                  
013100 01  DYNAMIC-SUBPROGRAMS.                                                 
013200     03 V16194                    PIC X(8)  VALUE 'V16194  '.             
013300     03 V16195                    PIC X(8)  VALUE 'V16195  '.             
013400     03 V16198                    PIC X(8)  VALUE 'V16198  '.             
013500 01  VGETLID-PARMS.                                                       
013600     03 VGETLID-LENGTH            PIC S9(4) COMP VALUE ZERO.              
013700     03 VGETLID-LOGONID           PIC X(8)  VALUE SPACE.                  
013800     EJECT                                                                
013900*-------------------------------- DB2 FELHANTERING                        
014000*    -COPY V161WS                                                         
014100*++INCLUDE V161WS                                                         
014200*-------------------------------- DB2-AREOR                               
014300     EXEC SQL                                                             
014400          INCLUDE SQLCA                                                   
014500     END-EXEC.                                                            
014600     EXEC SQL                                                             
014700          INCLUDE EXTRACT                                                 
014800     END-EXEC.                                                            
014900*    -COPY EXTRACT -PRE EXTRACT-                                          
015000     EJECT                                                                
015100*--------------------------------------------------------------           
015200 LINKAGE SECTION.                                                         
015300*--------------------------------------------------------------           
015400 01 PARM.                                                                 
015500     03 PARM-EXTRACTID         PIC X(8).                                  
015600*************************************************************             
015700 PROCEDURE DIVISION USING PARM.                                           
015800*************************************************************             
015900     PERFORM A-INIT                                                       
016000     IF SW-ERROR = NOO                                                    
016100       PERFORM B-CHECK-AUTHORIZATION                                      
016200       IF SW-ERROR = NOO                                                  
016300         PERFORM C-SELECT-PANELINFO                                       
016400         IF SW-ERROR = NOO                                                
016500           MOVE PARM-EXTRACTID TO W-EXTRACTID                             
016600           CALL V16198 USING W-EXTRACTID                                  
016700           PERFORM D-DISPLAY-PANEL                                        
016800           IF SW-ENTER-PRESSED = YES                                      
016900             PERFORM E-UPDATE-EXTRACTID                                   
017000           END-IF                                                         
017100         END-IF                                                           
017200       END-IF                                                             
017300     END-IF                                                               
017400     PERFORM Z-FINIT                                                      
017500     GOBACK                                                               
017600     CONTINUE.                                                            
017700                                                                          
017800*--------------------------------------------------------------           
017900 A-INIT SECTION.                                                          
018000*--------------------------------------------------------------           
018100     MOVE 'A-INIT                  ' TO ABEND-SECTION                     
018200D    DISPLAY ABEND-SECTION                                                
018300     SKIP2                                                                
018400     MOVE NOO TO SW-ERROR                                                 
018500     PERFORM AA-INIT-PARMS                                                
018600     PERFORM AB-VDEF-PANEL-V161603M                                       
018700     CONTINUE.                                                            
018800     EJECT                                                                
018900*--------------------------------------------------------------           
019000 AA-INIT-PARMS    SECTION.                                                
019100*--------------------------------------------------------------           
019200     MOVE 'AA-INIT-PARMS           ' TO ABEND-SECTION                     
019300D    DISPLAY ABEND-SECTION                                                
019400     SKIP2                                                                
019500D    DISPLAY 'PARM-EXTRACTID    :' PARM-EXTRACTID                         
019600     IF PARM-EXTRACTID = SPACE                                            
019700        MOVE RCODE-12 TO RCODE                                            
019800        MOVE YES TO SW-ERROR                                              
019900     END-IF                                                               
020000     CONTINUE.                                                            
020100     EJECT                                                                
020200*--------------------------------------------------------------           
020300 AB-VDEF-PANEL-V161603M SECTION.                                          
020400*--------------------------------------------------------------           
020500     MOVE 'AB-VDEF-PANEL-V161603M    ' TO ABEND-SECTION                   
020600D    DISPLAY ABEND-SECTION                                                
020700     SKIP2                                                                
020800     CALL 'ISPLINK' USING VDEFINE N-EXTRACTID ISPF-EXTRACTID              
020900                          CHAR LTH-EXTRACTID                              
021000     CALL 'ISPLINK' USING VDEFINE N-TYPE      ISPF-TYPE                   
021100                          CHAR LTH-TYPE                                   
021200     CALL 'ISPLINK' USING VDEFINE N-COPYTEXT  ISPF-COPYTEXT               
021300                          CHAR LTH-COPYTEXT                               
021400     CALL 'ISPLINK' USING VDEFINE N-DESCRIPTION ISPF-DESCRIPTION          
021500                          CHAR LTH-DESCRIPTION                            
021600     CALL 'ISPLINK' USING VDEFINE N-SIZE ISPF-SIZE                        
021700                          CHAR  LTH-SIZE                                  
021800     CALL 'ISPLINK' USING VDEFINE N-VALID-NO-DAYS                         
021900                                  ISPF-VALID-NO-DAYS                      
022000                                  CHAR                                    
022100                                  LTH-VALID-NO-DAYS                       
022200     CALL 'ISPLINK' USING VDEFINE N-EXTRACT-FILE ISPF-EXTRACT-FILE        
022300                          CHAR  LTH-EXTRACT-FILE                          
022400     CALL 'ISPLINK' USING VDEFINE N-EXTRACT-JOB ISPF-EXTRACT-JOB          
022500                          CHAR  LTH-EXTRACT-JOB                           
022600     CALL 'ISPLINK' USING VDEFINE N-JOB-TYPE    ISPF-JOB-TYPE             
022700                          CHAR  LTH-JOB-TYPE                              
022800                                                                          
022900     CONTINUE.                                                            
023000     EJECT                                                                
023100*--------------------------------------------------------------           
023200 B-CHECK-AUTHORIZATION SECTION.                                           
023300*--------------------------------------------------------------           
023400     MOVE 'B-CHECK-AUTHORIZATION  ' TO ABEND-SECTION                      
023500D    DISPLAY ABEND-SECTION                                                
023600     SKIP2                                                                
023700     CALL V16194 USING PARM-EXTRACTID                                     
023800     IF RETURN-CODE = 0                                                   
023900        MOVE NOO TO SW-ERROR                                              
024000     ELSE                                                                 
024100        MOVE YES TO SW-ERROR                                              
024200        MOVE RCODE-8 TO RCODE                                             
024300     END-IF                                                               
024400     CONTINUE.                                                            
024500     EJECT                                                                
024600*--------------------------------------------------------------           
024700 C-SELECT-PANELINFO SECTION.                                              
024800*--------------------------------------------------------------           
024900     MOVE 'C-SELECT-PANELINFO   ' TO ABEND-SECTION                        
025000D    DISPLAY ABEND-SECTION                                                
025100     SKIP2                                                                
025200     MOVE PARM-EXTRACTID TO EXTRACT-EXTRACTID                             
025300D    DISPLAY EXTRACT-EXTRACTID                                            
025400     PERFORM SQL-SELECT-EXTRACTID                                         
025500     IF SQLCODE NOT = +000                                                
025600        MOVE YES TO SW-ERROR                                              
025700        IF SQLCODE = +100                                                 
025800          MOVE RCODE-8 TO RCODE                                           
025900        END-IF                                                            
026000     END-IF                                                               
026100     CONTINUE.                                                            
026200     EJECT                                                                
026300*--------------------------------------------------------------           
026400 D-DISPLAY-PANEL SECTION.                                                 
026500*--------------------------------------------------------------           
026600     MOVE 'D-DISPLAY-PANEL         ' TO ABEND-SECTION                     
026700D    DISPLAY ABEND-SECTION                                                
026800     SKIP2                                                                
026900     MOVE N-TYPE        TO ISPF-CURSOR                                    
027000     MOVE EXTRACT-EXTRACTID     TO ISPF-EXTRACTID                         
027100     MOVE EXTRACT-TYPEX         TO ISPF-TYPE                              
027200     MOVE EXTRACT-COPYTEXT      TO ISPF-COPYTEXT                          
027300     MOVE EXTRACT-DESCRIPTION   TO ISPF-DESCRIPTION                       
027400     MOVE EXTRACT-SIZEX         TO W-SIZE                                 
027500     MOVE W-SIZE                TO ISPF-SIZE                              
027600     MOVE EXTRACT-VALID-NO-DAYS TO W-VALID-NO-DAYS                        
027700     MOVE W-VALID-NO-DAYS       TO ISPF-VALID-NO-DAYS                     
027800     MOVE EXTRACT-EXTRACT-FILE  TO ISPF-EXTRACT-FILE                      
027900     MOVE EXTRACT-EXTRACT-JOB   TO ISPF-EXTRACT-JOB                       
028000     MOVE EXTRACT-JOB-TYPE      TO ISPF-JOB-TYPE                          
028100     PERFORM SQL-COMMIT                                                   
028200     PERFORM S01-DISPLAY-PANEL                                            
028300     IF RCODE = 0                                                         
028400        MOVE YES TO SW-ENTER-PRESSED                                      
028500     ELSE                                                                 
028600        MOVE NOO TO SW-ENTER-PRESSED                                      
028700        IF RCODE = 8                                                      
028800           MOVE RCODE-4 TO RCODE                                          
028900        END-IF                                                            
029000     END-IF                                                               
029100     CONTINUE.                                                            
029200     EJECT                                                                
029300*--------------------------------------------------------------           
029400 E-UPDATE-EXTRACTID SECTION.                                              
029500*--------------------------------------------------------------           
029600     MOVE 'E-UPDATE-EXTRACTID      ' TO ABEND-SECTION                     
029700D    DISPLAY ABEND-SECTION                                                
029800     SKIP2                                                                
029900     MOVE ISPF-EXTRACTID TO   EXTRACT-EXTRACTID                           
030000     MOVE ISPF-TYPE      TO   EXTRACT-TYPEX                               
030100     MOVE ISPF-COPYTEXT  TO   EXTRACT-COPYTEXT                            
030200     MOVE ISPF-DESCRIPTION TO EXTRACT-DESCRIPTION                         
030300     PERFORM EA-EDIT-NUM-FIELD                                            
030400     MOVE ISPF-SIZE        TO W-SIZE                                      
030500     MOVE ISPF-VALID-NO-DAYS TO W-VALID-NO-DAYS                           
030600     MOVE W-SIZE           TO EXTRACT-SIZEX                               
030700     MOVE W-VALID-NO-DAYS TO EXTRACT-VALID-NO-DAYS                        
030800     MOVE ISPF-EXTRACT-FILE TO EXTRACT-EXTRACT-FILE                       
030900     MOVE ISPF-EXTRACT-JOB TO EXTRACT-EXTRACT-JOB                         
031000     MOVE ISPF-JOB-TYPE    TO EXTRACT-JOB-TYPE                            
031100     CALL V16195 USING VGETLID-LENGTH                                     
031200                        VGETLID-LOGONID                                   
031300     MOVE VGETLID-LOGONID TO EXTRACT-OWNER                                
031400     PERFORM SQL-UPDATE-EXTRACTID                                         
031500     IF SQLCODE NOT = +000                                                
031600        MOVE YES TO SW-ERROR                                              
031700        IF SQLCODE = +100                                                 
031800           MOVE RCODE-4 TO RCODE                                          
031900        END-IF                                                            
032000     END-IF                                                               
032100                                                                          
032200     CONTINUE.                                                            
032300     EJECT                                                                
032400*--------------------------------------------------------------           
032500 EA-EDIT-NUM-FIELD SECTION.                                               
032600*--------------------------------------------------------------           
032700     MOVE 'EA-EDIT-NUM-FIELD     ' TO ABEND-SECTION                       
032800D    DISPLAY ABEND-SECTION                                                
032900     SKIP2                                                                
033000*HÖGERJUSTERA ISPF-SIZE                                                   
033100     MOVE ISPF-SIZE TO W-JUST-TAB1                                        
033200     MOVE SPACE        TO W-JUST-TAB2                                     
033300     SET IX-TAB1 TO +5                                                    
033400     SET IX-TAB2 TO +5                                                    
033500     PERFORM UNTIL (IX-TAB1 = 0)                                          
033600         IF W-JUST-1 (IX-TAB1) NOT = SPACE                                
033700            MOVE W-JUST-1 (IX-TAB1) TO W-JUST-2 (IX-TAB2)                 
033800            SET IX-TAB2 DOWN BY +1                                        
033900         END-IF                                                           
034000         SET IX-TAB1 DOWN BY +1                                           
034100     END-PERFORM                                                          
034200     MOVE W-JUST-TAB2 TO ISPF-SIZE                                        
034300*HÖGERJUSTERA ISPF-VALID-NO-DAYS                                          
034400     MOVE ISPF-VALID-NO-DAYS TO W-JUST-TAB1                               
034500     MOVE SPACE            TO W-JUST-TAB2                                 
034600     SET IX-TAB1 TO +3                                                    
034700     SET IX-TAB2 TO +3                                                    
034800     PERFORM UNTIL (IX-TAB1 = 0)                                          
034900         IF W-JUST-1 (IX-TAB1) NOT = SPACE                                
035000            MOVE W-JUST-1 (IX-TAB1) TO W-JUST-2 (IX-TAB2)                 
035100            SET IX-TAB2 DOWN BY +1                                        
035200         END-IF                                                           
035300         SET IX-TAB1 DOWN BY +1                                           
035400     END-PERFORM                                                          
035500     MOVE W-JUST-TAB2 TO ISPF-VALID-NO-DAYS                               
035600     CONTINUE.                                                            
035700     EJECT                                                                
035800*--------------------------------------------------------------           
035900 Z-FINIT SECTION.                                                         
036000*--------------------------------------------------------------           
036100     MOVE 'Z-FINIT                  ' TO ABEND-SECTION                    
036200D    DISPLAY ABEND-SECTION                                                
036300     SKIP2                                                                
036400     PERFORM ZA-VDEL-PANEL-V161603M                                       
036500     MOVE RCODE TO RETURN-CODE                                            
036600     CONTINUE.                                                            
036700     EJECT                                                                
036800*--------------------------------------------------------------           
036900 ZA-VDEL-PANEL-V161603M SECTION.                                          
037000*--------------------------------------------------------------           
037100     MOVE 'ZA-VDEL-PANEL-V161603M    ' TO ABEND-SECTION                   
037200D    DISPLAY ABEND-SECTION                                                
037300     SKIP2                                                                
037400     CALL 'ISPLINK' USING VDELETE N-EXTRACTID                             
037500     CALL 'ISPLINK' USING VDELETE N-TYPE                                  
037600     CALL 'ISPLINK' USING VDELETE N-COPYTEXT                              
037700     CALL 'ISPLINK' USING VDELETE N-DESCRIPTION                           
037800     CALL 'ISPLINK' USING VDELETE N-SIZE                                  
037900     CALL 'ISPLINK' USING VDELETE N-VALID-NO-DAYS                         
038000     CALL 'ISPLINK' USING VDELETE N-EXTRACT-FILE                          
038100     CALL 'ISPLINK' USING VDELETE N-EXTRACT-JOB                           
038200     CALL 'ISPLINK' USING VDELETE N-JOB-TYPE                              
038300     CONTINUE.                                                            
038400     EJECT                                                                
038500*--------------------------------------------------------------           
038600 S01-DISPLAY-PANEL SECTION.                                               
038700*--------------------------------------------------------------           
038800     MOVE 'S01-DISPLAY-PANEL           ' TO ABEND-SECTION                 
038900D    DISPLAY ABEND-SECTION                                                
039000     SKIP2                                                                
039100     CALL 'ISPLINK' USING DISPLAYE V161603M                               
039200                          ISPF-MSG-ID ISPF-CURSOR                         
039300     MOVE RETURN-CODE TO RCODE RCODE-DISPL                                
039400     IF RCODE > +12                                                       
039500        PERFORM S99-ERROR-ROUTINE                                         
039600        MOVE YES TO SW-ERROR                                              
039700        MOVE RCODE-16 TO RCODE                                            
039800     END-IF                                                               
039900     CONTINUE.                                                            
040000     EJECT                                                                
040100*--------------------------------------------------------------           
040200 S99-ERROR-ROUTINE SECTION.                                               
040300*--------------------------------------------------------------           
040400     SKIP2                                                                
040500     STRING 'ERROR IN ' ABEND-SECTION ' RCODE ='                          
040600     RCODE-DISPL DELIMITED BY SIZE                                        
040700     INTO W-ERROR-MESSAGE                                                 
040800     DISPLAY W-ERROR-MESSAGE                                              
040900     CONTINUE.                                                            
041000     EJECT                                                                
041100*--------------------------------------------------------------           
041200 SQL-SELECT-EXTRACTID SECTION.                                            
041300*--------------------------------------------------------------           
041400     MOVE 'SQL-SELECT-EXTRACTID        ' TO ABEND-SECTION                 
041500D    DISPLAY ABEND-SECTION                                                
041600     SKIP2                                                                
041700     EXEC SQL                                                             
041800        SELECT  TYPE, COPYTEXT, DESCRIPTION, SIZE,                        
041900                VALID_NO_DAYS, EXTRACT_FILE,                              
042000                EXTRACT_JOB, JOB_TYPE                                     
042100        INTO :EXTRACT-TYPEX,                                              
042200             :EXTRACT-COPYTEXT:W-NULL-INDIKATOR1,                         
042300             :EXTRACT-DESCRIPTION,                                        
042400             :EXTRACT-SIZEX,                                              
042500             :EXTRACT-VALID-NO-DAYS,                                      
042600             :EXTRACT-EXTRACT-FILE,                                       
042700             :EXTRACT-EXTRACT-JOB,                                        
042800             :EXTRACT-JOB-TYPE                                            
042900        FROM EXTRACT                                                      
043000        WHERE EXTRACTID = :EXTRACT-EXTRACTID                              
043100     END-EXEC                                                             
043200     IF SQLCODE = +000 AND W-NULL-INDIKATOR1 < 0                          
043300        MOVE SPACE TO EXTRACT-COPYTEXT                                    
043400     END-IF                                                               
043500     PERFORM S95-CONTROL-SQLCODE                                          
043600     CONTINUE.                                                            
043700     EJECT                                                                
043800*--------------------------------------------------------------           
043900 SQL-UPDATE-EXTRACTID SECTION.                                            
044000*--------------------------------------------------------------           
044100     MOVE 'SQL-UPDATE-EXTRACTID        ' TO ABEND-SECTION                 
044200D    DISPLAY ABEND-SECTION                                                
044300     SKIP2                                                                
044400     EXEC SQL                                                             
044500        UPDATE EXTRACT                                                    
044600        SET TYPE         = :EXTRACT-TYPEX,                                
044700            COPYTEXT     = :EXTRACT-COPYTEXT,                             
044800            DESCRIPTION  = :EXTRACT-DESCRIPTION,                          
044900            SIZE         = :EXTRACT-SIZEX,                                
045000            VALID_NO_DAYS = :EXTRACT-VALID-NO-DAYS,                       
045100            EXTRACT_FILE = :EXTRACT-EXTRACT-FILE,                         
045200            EXTRACT_JOB  = :EXTRACT-EXTRACT-JOB,                          
045300            JOB_TYPE     = :EXTRACT-JOB-TYPE,                             
045400            OWNER        = :EXTRACT-OWNER                                 
045500        WHERE EXTRACTID  = :EXTRACT-EXTRACTID                             
045600     END-EXEC                                                             
045700     PERFORM S95-CONTROL-SQLCODE                                          
045800     CONTINUE.                                                            
045900     EJECT                                                                
046000*--------------------------------------------------------------           
046100 SQL-COMMIT SECTION.                                                      
046200*--------------------------------------------------------------           
046300     MOVE 'SQL-COMMIT                  ' TO ABEND-SECTION                 
046400D    DISPLAY ABEND-SECTION                                                
046500     SKIP2                                                                
046600     EXEC SQL                                                             
046700        COMMIT                                                            
046800     END-EXEC                                                             
046900     PERFORM S95-CONTROL-SQLCODE                                          
047000     CONTINUE.                                                            
047100     EJECT                                                                
047200*-------------------------------- DB2 ERROR HANDLING                      
047300*    -COPY V161PS                                                         
047400*++INCLUDE V161PS                                                         
