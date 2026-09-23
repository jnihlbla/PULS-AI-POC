000100***************************************************************           
000200 ID  DIVISION.                                                            
000300***************************************************************           
000400 PROGRAM-ID.    V1616020.                                                 
000500 AUTHOR.        CARINA HOLMQVIST                                          
000600 DATE-WRITTEN.  SEPTEMBER 1991                                            
000700***************************************************************           
000800*                                                                         
000900*REMARKS:                                                                 
001000*                                                                         
001100*  * CODE N (NEW)    ON EXTRACTADM PANEL                                  
001200*                                                                         
001300*  * DB2-TABLES:                                                          
001400*                                                                         
001500*        EXTRACT                                                          
001600*                                                                         
001700*  * SUBPROGRAM:                                                          
001800*                                                                         
001900*        V16195 - RETURNS LOGONID FOR CURRENT USER (VGETLID)              
002000*                                                                         
002100*  * PARMS:                                                               
002200*                                                                         
002300*        EXTRACTID                                                        
002400*                                                                         
002500*  * RETURNCODES:                                                         
002600*                                                                         
002700*         4 - FUNCTION CREATE CANCELLED                                   
002800*         8 - EXTRACTID ALREADY EXISTS                                    
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
005200 01  PROGRAM-NAME                 PIC X(8)  VALUE 'V1616020'.             
005300 01  ABEND-SECTION                PIC X(25) VALUE SPACE.                  
005400 01  RETURN-CODES.                                                        
005500     03  RCODE                    PIC S9(4) COMP SYNC VALUE ZERO.         
005600     03  RCODE-4                  PIC S9(4) COMP SYNC VALUE 4.            
005700     03  RCODE-8                  PIC S9(4) COMP SYNC VALUE 8.            
005800     03  RCODE-7                  PIC S9(4) COMP SYNC VALUE 7.            
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
007300     03 W-ERROR-MESSAGE           PIC X(40) VALUE SPACE.                  
007400     03 W-JUST-TAB1.                                                      
007500        05 W-JUST-1               PIC X(1) OCCURS 5                       
007600                                  INDEXED BY IX-TAB1.                     
007700     03 W-JUST-TAB2.                                                      
007800        05 W-JUST-2               PIC X(1) OCCURS 5                       
007900                                  INDEXED BY IX-TAB2.                     
008000     SKIP2                                                                
008100 01  N-PANEL-V161602M.                                                    
008200     03 N-EXTRACTID               PIC X(8)  VALUE 'V161602A'.             
008300     03 N-TYPE                    PIC X(8)  VALUE 'V161602B'.             
008400     03 N-COPYTEXT                PIC X(8)  VALUE 'V161602C'.             
008500     03 N-DESCRIPTION             PIC X(8)  VALUE 'V161602D'.             
008600     03 N-SIZE                    PIC X(8)  VALUE 'V161602E'.             
008700     03 N-VALID-NO-DAYS           PIC X(8)  VALUE 'V161602F'.             
008800     03 N-EXTRACT-FILE            PIC X(8)  VALUE 'V161602G'.             
008900     03 N-EXTRACT-JOB             PIC X(8)  VALUE 'V161602H'.             
009000     03 N-JOB-TYPE                PIC X(8)  VALUE 'V161602I'.             
009100     SKIP2                                                                
009200 01  LTH-PANEL-V161602M.                                                  
009300     03 LTH-EXTRACTID             PIC S9(6) VALUE 8  COMP.                
009400     03 LTH-TYPE                  PIC S9(6) VALUE 8  COMP.                
009500     03 LTH-COPYTEXT              PIC S9(6) VALUE 8  COMP.                
009600     03 LTH-DESCRIPTION           PIC S9(6) VALUE 25 COMP.                
009700     03 LTH-SIZE                  PIC S9(6) VALUE 5  COMP.                
009800     03 LTH-VALID-NO-DAYS         PIC S9(6) VALUE 3  COMP.                
009900     03 LTH-EXTRACT-FILE          PIC S9(6) VALUE 42 COMP.                
010000     03 LTH-EXTRACT-JOB           PIC S9(6) VALUE 52 COMP.                
010100     03 LTH-JOB-TYPE              PIC S9(6) VALUE 1  COMP.                
010200     SKIP2                                                                
010300 01  ISPF-PANEL-V161602M.                                                 
010400     03 ISPF-EXTRACTID            PIC X(8)  VALUE SPACE.                  
010500     03 ISPF-TYPE                 PIC X(8)  VALUE SPACE.                  
010600     03 ISPF-COPYTEXT             PIC X(8)  VALUE SPACE.                  
010700     03 ISPF-DESCRIPTION          PIC X(25) VALUE SPACE.                  
010800     03 ISPF-SIZE                 PIC X(5)  VALUE SPACE.                  
010900     03 ISPF-VALID-NO-DAYS        PIC X(3)  VALUE SPACE.                  
011000     03 ISPF-EXTRACT-FILE         PIC X(42) VALUE SPACE.                  
011100     03 ISPF-EXTRACT-JOB          PIC X(52) VALUE SPACE.                  
011200     03 ISPF-JOB-TYPE             PIC X(1)  VALUE SPACE.                  
011300     SKIP2                                                                
011400 01  W-SIZE                       PIC ZZZZZ.                              
011500 01  W-VALID-NO-DAYS              PIC ZZZ.                                
011600*--------------------------------------------------------------           
011700*ISPF-CONSTANTS                                                           
011800*--------------------------------------------------------------           
011900 01  DISPLAYE                     PIC X(8)  VALUE 'DISPLAY '.             
012000 01  VDEFINE                      PIC X(8)  VALUE 'VDEFINE '.             
012100 01  VDELETE                      PIC X(8)  VALUE 'VDELETE '.             
012200 01  CHAR                         PIC X(8)  VALUE 'CHAR    '.             
012300*--------------------------------------------------------------           
012400*ISPF-FIELDS                                                              
012500*--------------------------------------------------------------           
012600 01  V161602M                     PIC X(8)  VALUE 'V1616020'.             
012700 01  ISPF-CURSOR                  PIC X(8)  VALUE SPACE.                  
012800 01  ISPF-MSG-ID                  PIC X(8)  VALUE SPACE.                  
012900 01  DYNAMIC-SUBPROGRAMS.                                                 
013000     03 V16195                    PIC X(8)  VALUE 'V16195  '.             
013100     03 V16198                    PIC X(8)  VALUE 'V16198  '.             
013200     03 V1616060                  PIC X(8)  VALUE 'V1616060'.             
013300 01  VGETLID-PARMS.                                                       
013400     03 VGETLID-LENGTH            PIC S9(4) COMP VALUE ZERO.              
013500     03 VGETLID-LOGONID           PIC X(8)  VALUE SPACE.                  
013600     EJECT                                                                
013700*-------------------------------- DB2 FELHANTERING                        
013800*    -COPY V161WS                                                         
013900*++INCLUDE V161WS                                                         
014000*-------------------------------- DB2-AREOR                               
014100     EXEC SQL                                                             
014200          INCLUDE SQLCA                                                   
014300     END-EXEC.                                                            
014400     EXEC SQL                                                             
014500          INCLUDE EXTRACT                                                 
014600     END-EXEC.                                                            
014700*    -COPY EXTRACT -PRE EXTRACT-                                          
014800*++INCLUDE EXTRACT                                                        
014900     EJECT                                                                
015000*--------------------------------------------------------------           
015100 LINKAGE SECTION.                                                         
015200*--------------------------------------------------------------           
015300 01 PARM.                                                                 
015400     03 PARM-EXTRACTID         PIC X(8).                                  
015500*************************************************************             
015600 PROCEDURE DIVISION USING PARM.                                           
015700*************************************************************             
015800     PERFORM A-INIT                                                       
015900     IF SW-ERROR = NOO                                                    
016000       PERFORM B-CHECK-EXTRACTID                                          
016100       IF SW-ERROR = NOO                                                  
016200          CALL V16198 USING PARM-EXTRACTID                                
016300          PERFORM C-DISPLAY-PANEL                                         
016400          IF SW-ENTER-PRESSED = YES                                       
016500             PERFORM D-INSERT-EXTRACTID                                   
016600          END-IF                                                          
016700       END-IF                                                             
016800     END-IF                                                               
016900     PERFORM Z-FINIT                                                      
017000     GOBACK                                                               
017100     CONTINUE.                                                            
017200                                                                          
017300*--------------------------------------------------------------           
017400 A-INIT SECTION.                                                          
017500*--------------------------------------------------------------           
017600     MOVE 'A-INIT                  ' TO ABEND-SECTION                     
017700D    DISPLAY ABEND-SECTION                                                
017800     SKIP2                                                                
017900     MOVE NOO TO SW-ERROR                                                 
018000     PERFORM AA-INIT-PARMS                                                
018100     PERFORM AB-VDEF-PANEL-V161602M                                       
018200     CONTINUE.                                                            
018300     EJECT                                                                
018400*--------------------------------------------------------------           
018500 AA-INIT-PARMS SECTION.                                                   
018600*--------------------------------------------------------------           
018700     MOVE 'AA-INIT-PARMS           ' TO ABEND-SECTION                     
018800D    DISPLAY ABEND-SECTION                                                
018900     SKIP2                                                                
019000D    DISPLAY 'PARM-EXTRACTID    :' PARM-EXTRACTID                         
019100     IF PARM-EXTRACTID = SPACE                                            
019200        MOVE RCODE-12 TO RCODE                                            
019300        MOVE YES TO SW-ERROR                                              
019400     END-IF                                                               
019500     CONTINUE.                                                            
019600     EJECT                                                                
019700*--------------------------------------------------------------           
019800 AB-VDEF-PANEL-V161602M SECTION.                                          
019900*--------------------------------------------------------------           
020000     MOVE 'AB-VDEF-PANEL-V161602M  ' TO ABEND-SECTION                     
020100D    DISPLAY ABEND-SECTION                                                
020200     SKIP2                                                                
020300     CALL 'ISPLINK' USING VDEFINE N-EXTRACTID ISPF-EXTRACTID              
020400                          CHAR LTH-EXTRACTID                              
020500     CALL 'ISPLINK' USING VDEFINE N-TYPE      ISPF-TYPE                   
020600                          CHAR LTH-TYPE                                   
020700     CALL 'ISPLINK' USING VDEFINE N-COPYTEXT  ISPF-COPYTEXT               
020800                          CHAR LTH-COPYTEXT                               
020900     CALL 'ISPLINK' USING VDEFINE N-DESCRIPTION ISPF-DESCRIPTION          
021000                          CHAR LTH-DESCRIPTION                            
021100     CALL 'ISPLINK' USING VDEFINE N-SIZE ISPF-SIZE                        
021200                          CHAR  LTH-SIZE                                  
021300     CALL 'ISPLINK' USING VDEFINE N-VALID-NO-DAYS                         
021400                                  ISPF-VALID-NO-DAYS                      
021500                                  CHAR                                    
021600                                  LTH-VALID-NO-DAYS                       
021700     CALL 'ISPLINK' USING VDEFINE N-EXTRACT-FILE ISPF-EXTRACT-FILE        
021800                          CHAR  LTH-EXTRACT-FILE                          
021900     CALL 'ISPLINK' USING VDEFINE N-EXTRACT-JOB ISPF-EXTRACT-JOB          
022000                          CHAR  LTH-EXTRACT-JOB                           
022100     CALL 'ISPLINK' USING VDEFINE N-JOB-TYPE    ISPF-JOB-TYPE             
022200                          CHAR  LTH-JOB-TYPE                              
022300                                                                          
022400     CONTINUE.                                                            
022500     EJECT                                                                
022600*--------------------------------------------------------------           
022700 B-CHECK-EXTRACTID SECTION.                                               
022800*--------------------------------------------------------------           
022900     MOVE 'B-CHECK-EXTRACTID' TO ABEND-SECTION                            
023000D    DISPLAY ABEND-SECTION                                                
023100     SKIP2                                                                
023200     MOVE PARM-EXTRACTID TO EXTRACT-EXTRACTID                             
023300     CALL V1616060 USING EXTRACT-EXTRACTID                                
023400     IF RETURN-CODE > +0                                                  
023500        MOVE YES TO SW-ERROR                                              
023600        MOVE RETURN-CODE TO RCODE                                         
023700     ELSE                                                                 
023800        PERFORM SQL-SELECT-EXTRACTID                                      
023900        IF SQLCODE = +000                                                 
024000           MOVE YES TO SW-ERROR                                           
024100           MOVE RCODE-8 TO RCODE                                          
024200        END-IF                                                            
024300     END-IF                                                               
024400     CONTINUE.                                                            
024500     EJECT                                                                
024600*--------------------------------------------------------------           
024700 C-DISPLAY-PANEL SECTION.                                                 
024800*--------------------------------------------------------------           
024900     MOVE 'C-DISPLAY-PANEL         ' TO ABEND-SECTION                     
025000D    DISPLAY ABEND-SECTION                                                
025100     SKIP2                                                                
025200     PERFORM SQL-COMMIT                                                   
025300     MOVE N-TYPE        TO ISPF-CURSOR                                    
025400     MOVE PARM-EXTRACTID TO ISPF-EXTRACTID                                
025500     PERFORM S01-DISPLAY-PANEL                                            
025600     IF RCODE = 0                                                         
025700        MOVE YES TO SW-ENTER-PRESSED                                      
025800     ELSE                                                                 
025900        MOVE NOO TO SW-ENTER-PRESSED                                      
026000        IF RCODE = 8                                                      
026100           MOVE RCODE-4 TO RCODE                                          
026200        END-IF                                                            
026300     END-IF                                                               
026400     CONTINUE.                                                            
026500     EJECT                                                                
026600*--------------------------------------------------------------           
026700 D-INSERT-EXTRACTID SECTION.                                              
026800*--------------------------------------------------------------           
026900     MOVE 'D-INSERT-EXTRACTID      ' TO ABEND-SECTION                     
027000D    DISPLAY ABEND-SECTION                                                
027100     SKIP2                                                                
027200     MOVE ISPF-EXTRACTID TO   EXTRACT-EXTRACTID                           
027300     MOVE ISPF-TYPE      TO   EXTRACT-TYPEX                               
027400     MOVE ISPF-COPYTEXT  TO   EXTRACT-COPYTEXT                            
027500     MOVE ISPF-DESCRIPTION TO EXTRACT-DESCRIPTION                         
027600     PERFORM DA-EDIT-NUM-FALT                                             
027700     MOVE ISPF-SIZE        TO W-SIZE                                      
027800     MOVE ISPF-VALID-NO-DAYS TO W-VALID-NO-DAYS                           
027900     MOVE W-SIZE           TO EXTRACT-SIZEX                               
028000     MOVE W-VALID-NO-DAYS  TO EXTRACT-VALID-NO-DAYS                       
028100     MOVE ISPF-EXTRACT-FILE TO EXTRACT-EXTRACT-FILE                       
028200     MOVE ISPF-EXTRACT-JOB TO EXTRACT-EXTRACT-JOB                         
028300     MOVE ISPF-JOB-TYPE    TO EXTRACT-JOB-TYPE                            
028400     CALL V16195 USING VGETLID-LENGTH                                     
028500                         VGETLID-LOGONID                                  
028600     MOVE VGETLID-LOGONID TO EXTRACT-OWNER                                
028700     PERFORM SQL-INSERT-EXTRACTID                                         
028800     IF SQLCODE = +000                                                    
028900        PERFORM SQL-COMMIT                                                
029000     ELSE                                                                 
029100        MOVE YES TO SW-ERROR                                              
029200        IF SQLCODE = -803                                                 
029300           MOVE RCODE-8 TO RCODE                                          
029400        END-IF                                                            
029500     END-IF                                                               
029600                                                                          
029700     CONTINUE.                                                            
029800     EJECT                                                                
029900*--------------------------------------------------------------           
030000 DA-EDIT-NUM-FALT SECTION.                                                
030100*--------------------------------------------------------------           
030200     MOVE 'CA-EDIT-NUM-FALT     ' TO ABEND-SECTION                        
030300D    DISPLAY ABEND-SECTION                                                
030400     SKIP2                                                                
030500*HÖGERJUSTERING ISPF-SIZE                                                 
030600     MOVE ISPF-SIZE TO W-JUST-TAB1                                        
030700     MOVE SPACE        TO W-JUST-TAB2                                     
030800     SET IX-TAB1 TO +5                                                    
030900     SET IX-TAB2 TO +5                                                    
031000     PERFORM UNTIL (IX-TAB1 = 0)                                          
031100         IF W-JUST-1 (IX-TAB1) NOT = SPACE                                
031200            MOVE W-JUST-1 (IX-TAB1) TO W-JUST-2 (IX-TAB2)                 
031300            SET IX-TAB2 DOWN BY +1                                        
031400         END-IF                                                           
031500         SET IX-TAB1 DOWN BY +1                                           
031600     END-PERFORM                                                          
031700     MOVE W-JUST-TAB2 TO ISPF-SIZE                                        
031800*HÖGERJUSTERING ISPF-VALID-NO-DAYS                                        
031900     MOVE ISPF-VALID-NO-DAYS TO W-JUST-TAB1                               
032000     MOVE SPACE            TO W-JUST-TAB2                                 
032100     SET IX-TAB1 TO +3                                                    
032200     SET IX-TAB2 TO +3                                                    
032300     PERFORM UNTIL (IX-TAB1 = 0)                                          
032400         IF W-JUST-1 (IX-TAB1) NOT = SPACE                                
032500            MOVE W-JUST-1 (IX-TAB1) TO W-JUST-2 (IX-TAB2)                 
032600            SET IX-TAB2 DOWN BY +1                                        
032700         END-IF                                                           
032800         SET IX-TAB1 DOWN BY +1                                           
032900     END-PERFORM                                                          
033000     MOVE W-JUST-TAB2 TO ISPF-VALID-NO-DAYS                               
033100     CONTINUE.                                                            
033200     EJECT                                                                
033300*--------------------------------------------------------------           
033400 Z-FINIT SECTION.                                                         
033500*--------------------------------------------------------------           
033600     MOVE 'Z-FINIT                  ' TO ABEND-SECTION                    
033700D    DISPLAY ABEND-SECTION                                                
033800     SKIP2                                                                
033900     PERFORM ZA-VDEL-PANEL-V161602M                                       
034000     MOVE RCODE TO RETURN-CODE                                            
034100     CONTINUE.                                                            
034200     EJECT                                                                
034300*--------------------------------------------------------------           
034400 ZA-VDEL-PANEL-V161602M SECTION.                                          
034500*--------------------------------------------------------------           
034600     MOVE 'ZB-VDEL-PANEL-V161602M  ' TO ABEND-SECTION                     
034700D    DISPLAY ABEND-SECTION                                                
034800     SKIP2                                                                
034900     CALL 'ISPLINK' USING VDELETE N-EXTRACTID                             
035000     CALL 'ISPLINK' USING VDELETE N-TYPE                                  
035100     CALL 'ISPLINK' USING VDELETE N-COPYTEXT                              
035200     CALL 'ISPLINK' USING VDELETE N-DESCRIPTION                           
035300     CALL 'ISPLINK' USING VDELETE N-SIZE                                  
035400     CALL 'ISPLINK' USING VDELETE N-VALID-NO-DAYS                         
035500     CALL 'ISPLINK' USING VDELETE N-EXTRACT-FILE                          
035600     CALL 'ISPLINK' USING VDELETE N-EXTRACT-JOB                           
035700     CALL 'ISPLINK' USING VDELETE N-JOB-TYPE                              
035800     CONTINUE.                                                            
035900     EJECT                                                                
036000*--------------------------------------------------------------           
036100 S01-DISPLAY-PANEL SECTION.                                               
036200*--------------------------------------------------------------           
036300     MOVE 'S01-DISPLAY-PANEL           ' TO ABEND-SECTION                 
036400D    DISPLAY ABEND-SECTION                                                
036500     SKIP2                                                                
036600     CALL 'ISPLINK' USING DISPLAYE V161602M                               
036700                          ISPF-MSG-ID ISPF-CURSOR                         
036800     MOVE RETURN-CODE TO RCODE RCODE-DISPL                                
036900     IF RCODE > +12                                                       
037000        PERFORM S99-ERROR-ROUTINE                                         
037100        MOVE YES TO SW-ERROR                                              
037200        MOVE RCODE-16 TO RCODE                                            
037300     END-IF                                                               
037400     CONTINUE.                                                            
037500     EJECT                                                                
037600*--------------------------------------------------------------           
037700 S99-ERROR-ROUTINE SECTION.                                               
037800*--------------------------------------------------------------           
037900     SKIP2                                                                
038000     STRING 'ERROR IN ' ABEND-SECTION ' RCODE ='                          
038100     RCODE-DISPL DELIMITED BY SIZE                                        
038200     INTO W-ERROR-MESSAGE                                                 
038300     DISPLAY W-ERROR-MESSAGE                                              
038400     CONTINUE.                                                            
038500     EJECT                                                                
038600*--------------------------------------------------------------           
038700 SQL-SELECT-EXTRACTID SECTION.                                            
038800*--------------------------------------------------------------           
038900     MOVE 'SQL-SELECT-EXTRACTID        ' TO ABEND-SECTION                 
039000D    DISPLAY ABEND-SECTION                                                
039100     SKIP2                                                                
039200     EXEC SQL                                                             
039300        SELECT  EXTRACTID                                                 
039400        INTO :EXTRACT-EXTRACTID                                           
039500        FROM EXTRACT                                                      
039600        WHERE EXTRACTID = :EXTRACT-EXTRACTID                              
039700     END-EXEC                                                             
039800     PERFORM S95-CONTROL-SQLCODE                                          
039900     CONTINUE.                                                            
040000     EJECT                                                                
040100*--------------------------------------------------------------           
040200 SQL-INSERT-EXTRACTID SECTION.                                            
040300*--------------------------------------------------------------           
040400     MOVE 'SQL-INSERT-EXTRACTID        ' TO ABEND-SECTION                 
040500D    DISPLAY ABEND-SECTION                                                
040600     SKIP2                                                                
040700     EXEC SQL                                                             
040800        INSERT  INTO EXTRACT (EXTRACTID,                                  
040900                              TYPE,                                       
041000                              COPYTEXT,                                   
041100                              DESCRIPTION,                                
041200                              SIZE,                                       
041300                              VALID_NO_DAYS,                              
041400                              EXTRACT_FILE,                               
041500                              EXTRACT_JOB,                                
041600                              JOB_TYPE,                                   
041700                              OWNER)                                      
041800        VALUES               (:EXTRACT-EXTRACTID,                         
041900                              :EXTRACT-TYPEX,                             
042000                              :EXTRACT-COPYTEXT,                          
042100                              :EXTRACT-DESCRIPTION,                       
042200                              :EXTRACT-SIZEX,                             
042300                              :EXTRACT-VALID-NO-DAYS,                     
042400                              :EXTRACT-EXTRACT-FILE,                      
042500                              :EXTRACT-EXTRACT-JOB,                       
042600                              :EXTRACT-JOB-TYPE,                          
042700                              :EXTRACT-OWNER)                             
042800     END-EXEC                                                             
042900     PERFORM S95-CONTROL-SQLCODE                                          
043000     CONTINUE.                                                            
043100     EJECT                                                                
043200*--------------------------------------------------------------           
043300 SQL-COMMIT SECTION.                                                      
043400*--------------------------------------------------------------           
043500     MOVE 'SQL-COMMIT                  ' TO ABEND-SECTION                 
043600D    DISPLAY ABEND-SECTION                                                
043700     SKIP2                                                                
043800     EXEC SQL                                                             
043900         COMMIT                                                           
044000     END-EXEC                                                             
044100     PERFORM S95-CONTROL-SQLCODE                                          
044200     CONTINUE.                                                            
044300     EJECT                                                                
044400*-------------------------------- DB2 ERROR HANDLING                      
044500*    -COPY V161PS                                                         
044600*++INCLUDE V161PS                                                         
