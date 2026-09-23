000100***************************************************************           
000200 ID  DIVISION.                                                            
000300***************************************************************           
000400 PROGRAM-ID.    V1617000.                                                 
000500 AUTHOR.        CARINA HOLMQVIST                                          
000600 DATE-WRITTEN.  SEPTEMBER 1991                                            
000700***************************************************************           
000800*                                                                         
000900*REMARKS:                                                                 
001000*                                                                         
001100*  * ACTION CODE A - EXTRACT AUTHORITY PANEL                              
001200*                                                                         
001300*  * DB2-TABLES:                                                          
001400*                                                                         
001500*        EXTRACT EXTRACT_USER                                             
001600*                                                                         
001610*  * SUBPROGRAMS:                                                         
001620*                                                                         
001630*        V16172 V16173 V16175 V16193 V16194 V16197                        
001640*                                                                         
001650*  * PARMS:                                                               
001660*                                                                         
001670*        EXTRACTID X(8)                                                   
001680*                                                                         
001800*  * RETURNCODES:                                                         
001900*                                                                         
002000*         4 - EXTRACTID EXISTS ALREADY                                    
002100*         8 - NO AUTHORITY                                                
002200*        12 - INVALID PARMS                                               
002300*        16 - SEVERE ISPF-ERROR                                           
002400*        20 - SEVERE DB2-ERROR                                            
002500*                                                                         
002600***************************************************************           
002700     EJECT                                                                
002800***************************************************************           
002900 ENVIRONMENT DIVISION.                                                    
003000***************************************************************           
003100     SKIP2                                                                
003200*--------------------------------------------------------------           
003300 CONFIGURATION SECTION.                                                   
003400*--------------------------------------------------------------           
003500 SOURCE-COMPUTER. IBM-370.                                                
003600*SOURCE-COMPUTER. IBM-370 WITH DEBUGGING MODE.                            
003700     SKIP2                                                                
003800***************************************************************           
003900 DATA DIVISION.                                                           
004000***************************************************************           
004100     SKIP2                                                                
004200*--------------------------------------------------------------           
004300 WORKING-STORAGE SECTION.                                                 
004400*--------------------------------------------------------------           
004500 01  PROGRAM-NAME                 PIC X(8)  VALUE 'V1617000'.             
004600 01  ABEND-SECTION                PIC X(25) VALUE SPACE.                  
004700 01  RETURN-CODES.                                                        
004800     03  RCODE                    PIC S9(4) COMP SYNC VALUE ZERO.         
004900     03  RCODE-0                  PIC S9(4) COMP SYNC VALUE 0.            
005000     03  RCODE-4                  PIC S9(4) COMP SYNC VALUE 4.            
005100     03  RCODE-8                  PIC S9(4) COMP SYNC VALUE 8.            
005200     03  RCODE-10                 PIC S9(4) COMP SYNC VALUE 10.           
005300     03  RCODE-12                 PIC S9(4) COMP SYNC VALUE 12.           
005400     03  RCODE-16                 PIC S9(4) COMP SYNC VALUE 16.           
005500     03  RCODE-20                 PIC S9(4) COMP SYNC VALUE 20.           
005600     03  RCODE-DISPL              PIC Z(4)-.                              
005700     SKIP2                                                                
005800 01  GENERAL-CONSTANTS.                                                   
005900     03 YES                       PIC X(1)  VALUE 'Y'.                    
006000     03 NOO                       PIC X(1)  VALUE 'N'.                    
006100     SKIP2                                                                
006200 01  SWITCHES.                                                            
006300     03 SW-ENTER-PRESSED          PIC X(1)  VALUE 'N'.                    
006400     03 SW-ERROR                  PIC X(1)  VALUE 'N'.                    
006500     03 SW-USERID-LISTED          PIC X(1)  VALUE 'N'.                    
006600     SKIP2                                                                
006700 01  W-AREAS.                                                             
006800     03 W-ERROR-MESSAGE           PIC X(40) VALUE SPACE.                  
006900     03 W-EXTRACTID               PIC X(8)  VALUE SPACE.                  
007000     03 W-USERID                  PIC X(7)  VALUE SPACE.                  
007100     03 W-ZZLCMD                  PIC X(1)  VALUE SPACE.                  
007200     03 W-EMPTY-PANEL             PIC X(8)  VALUE SPACE.                  
007400     SKIP2                                                                
007500 01  N-GENERAL.                                                           
007600     03 N-SMSG                    PIC X(8)  VALUE 'SMSG    '.             
007700     03 N-LMSG                    PIC X(8)  VALUE 'LMSG    '.             
007800     03 N-ZCMD                    PIC X(8)  VALUE 'ZCMD    '.             
007900     03 N-ZTDSELS                 PIC X(8)  VALUE 'ZTDSELS '.             
008000     03 N-ZWINTTL                 PIC X(8)  VALUE 'ZWINTTL '.             
008010     03 N-ZZ1LCMD                 PIC X(8)  VALUE 'ZZ1LCMD '.             
008020     03 N-ZZLCMD                  PIC X(8)  VALUE 'ZZLCMD  '.             
008100     SKIP2                                                                
008200 01  N-PANEL-V161700M.                                                    
008300     03 N-EXTRACTID               PIC X(8)  VALUE 'V161700A'.             
008600     03 N-USERID-1                PIC X(8)  VALUE 'V161700B'.             
008700     03 N-USERID                  PIC X(8)  VALUE 'V161700C'.             
008800     03 N-AUTH                    PIC X(8)  VALUE 'V161700D'.             
008900     03 N-NAME                    PIC X(8)  VALUE 'V161700E'.             
009000     SKIP2                                                                
009100 01  LTH-GENERAL.                                                         
009200     03 LTH-SMSG                  PIC S9(6) VALUE 24 COMP.                
009300     03 LTH-LMSG                  PIC S9(6) VALUE 7  COMP.                
009400     03 LTH-ZCMD                  PIC S9(6) VALUE 50 COMP.                
009500     03 LTH-ZTDSELS               PIC S9(6) VALUE 4  COMP.                
009600     03 LTH-ZWINTTL               PIC S9(6) VALUE 30 COMP.                
009610     03 LTH-ZZ1LCMD               PIC S9(6) VALUE 1  COMP.                
009620     03 LTH-ZZLCMD                PIC S9(6) VALUE 1  COMP.                
009700     SKIP2                                                                
009800 01  LTH-PANEL-V161700M.                                                  
009900     03 LTH-EXTRACTID             PIC S9(6) VALUE 8  COMP.                
010200     03 LTH-USERID-1              PIC S9(6) VALUE 7  COMP.                
010300     03 LTH-USERID                PIC S9(6) VALUE 7  COMP.                
010400     03 LTH-AUTH                  PIC S9(6) VALUE 1  COMP.                
010500     03 LTH-NAME                  PIC S9(6) VALUE 20 COMP.                
010600     SKIP2                                                                
010700 01  ISPF-GENERAL.                                                        
010800     03 ISPF-SMSG                 PIC X(24) VALUE SPACE.                  
010900     03 ISPF-LMSG                 PIC X(74) VALUE SPACE.                  
011000     03 ISPF-ZCMD                 PIC X(50) VALUE SPACE.                  
011100     03 ISPF-ZTDSELS              PIC 9(4)  VALUE ZERO.                   
011200     03 ISPF-ZWINTTL              PIC X(30) VALUE ZERO.                   
011210     03 ISPF-ZZ1LCMD              PIC X(1)  VALUE SPACE.                  
011220     03 ISPF-ZZLCMD               PIC X(1)  VALUE SPACE.                  
011300 01  ISPF-PANEL-V161700M.                                                 
011400     03 ISPF-EXTRACTID            PIC X(8)  VALUE SPACE.                  
011700     03 ISPF-USERID-1             PIC X(7)  VALUE SPACE.                  
011800     03 ISPF-USERID               PIC X(7)  VALUE SPACE.                  
011900     03 ISPF-AUTH                 PIC X(1)  VALUE SPACE.                  
012000     03 ISPF-NAME                 PIC X(20) VALUE SPACE.                  
012100 01  DYNAMIC-SUBPROGRAMS.                                                 
012200     03 V161702P                  PIC X(8)  VALUE 'V1617020'.             
012300     03 V161703P                  PIC X(8)  VALUE 'V1617030'.             
012400     03 V161705P                  PIC X(8)  VALUE 'V1617050'.             
012401     03 V16193                    PIC X(8)  VALUE 'V16193  '.             
012402     03 V16194                    PIC X(8)  VALUE 'V16194  '.             
012410     03 V16197                    PIC X(8)  VALUE 'V16197  '.             
012411 01  V16197-PARMS.                                                        
012412     03 V16197-USERID             PIC X(7)  VALUE SPACE.                  
012413     03 V16197-NAME               PIC X(20) VALUE ZERO.                   
012500     SKIP2                                                                
012600*--------------------------------------------------------------           
012700*ISPF-CONSTANTS                                                           
012800*--------------------------------------------------------------           
012900 01  DISPLAYE                     PIC X(8)  VALUE 'DISPLAY '.             
013000 01  SELECTT                      PIC X(8)  VALUE 'SELECT  '.             
013100 01  ADDPOP                       PIC X(8)  VALUE 'ADDPOP  '.             
013200 01  REMPOP                       PIC X(8)  VALUE 'REMPOP  '.             
013300 01  CONTROLL                     PIC X(8)  VALUE 'CONTROL '.             
013400 01  SAVEE                        PIC X(8)  VALUE 'SAVE    '.             
013500 01  RESTOREE                     PIC X(8)  VALUE 'RESTORE '.             
013600 01  VDEFINE                      PIC X(8)  VALUE 'VDEFINE '.             
013700 01  VDELETE                      PIC X(8)  VALUE 'VDELETE '.             
013800 01  TBADD                        PIC X(8)  VALUE 'TBADD   '.             
013900 01  TBMOD                        PIC X(8)  VALUE 'TBMOD   '.             
014000 01  TBGET                        PIC X(8)  VALUE 'TBGET   '.             
014100 01  TBDELETE                     PIC X(8)  VALUE 'TBDELETE'.             
014200 01  TBTOP                        PIC X(8)  VALUE 'TBTOP   '.             
014300 01  TBCREATE                     PIC X(8)  VALUE 'TBCREATE'.             
014400 01  TBDISPL                      PIC X(8)  VALUE 'TBDISPL '.             
014500 01  TABNOW                       PIC X(8)  VALUE 'NOWRITE '.             
014600 01  TABREP                       PIC X(8)  VALUE 'REPLACE '.             
014700 01  CHAR                         PIC X(8)  VALUE 'CHAR    '.             
014800*--------------------------------------------------------------           
014900*ISPF-FIELD                                                               
015000*--------------------------------------------------------------           
015100 01  V161700M                     PIC X(8)   VALUE 'V1617000'.            
015300 01  V1617001                     PIC X(8)   VALUE 'V1617001'.            
015400 01  ISPF-PANEL                   PIC X(8)   VALUE SPACE.                 
015500 01  ISPF-ROW-POS                 PIC S9(6) COMP SYNC VALUE +2.           
015600 01  ISPF-COL-POS                 PIC S9(6) COMP SYNC VALUE +20.          
015700 01  ISPF-MSG-ID                  PIC X(8)   VALUE SPACE.                 
015800     EJECT                                                                
015900 01  EXTUSERTAB.                                                          
016000     03 EXTUSERTAB-NAME           PIC X(8)  VALUE 'V1617000'.             
016100     03 EXTUSERTAB-KEY            PIC X(10)  VALUE                        
016200     '(V161700C)'.                                                        
016300     03 EXTUSERTAB-VAR            PIC X(19) VALUE                         
016400     '(V161700D V161700E)'.                                               
016500 01  MESSAGES.                                                            
016600     03 MSG-VIOS101A              PIC X(8)  VALUE 'VIOS101A'.             
016700     03 MSG-VIOS101B              PIC X(8)  VALUE 'VIOS101B'.             
016800     03 MSG-VIOS101C              PIC X(8)  VALUE 'VIOS101C'.             
016900     03 MSG-VIOS101D              PIC X(8)  VALUE 'VIOS101D'.             
017000     03 MSG-VIOS101E              PIC X(8)  VALUE 'VIOS101E'.             
017100     03 MSG-VIOS101F              PIC X(8)  VALUE 'VIOS101F'.             
017200     03 MSG-VIOS101G              PIC X(8)  VALUE 'VIOS101G'.             
017300     03 MSG-VIOS101H              PIC X(8)  VALUE 'VIOS101H'.             
017400     03 MSG-VIOS101I              PIC X(8)  VALUE 'VIOS101I'.             
017500     03 MSG-VIOS101J              PIC X(8)  VALUE 'VIOS101J'.             
017600     03 MSG-VIOS101K              PIC X(8)  VALUE 'VIOS101K'.             
017700     03 MSG-VIOS101L              PIC X(8)  VALUE 'VIOS101L'.             
017800     03 MSG-VIOS101M              PIC X(8)  VALUE 'VIOS101M'.             
017900     03 MSG-VIOS101N              PIC X(8)  VALUE 'VIOS101N'.             
017910     03 MSG-VIOS101O              PIC X(8)  VALUE 'VIOS101O'.             
018000*-------------------------------- DB2 ERROR HANDLING                      
018100*    -COPY V161WS                                                         
018200*++INCLUDE V161WS                                                         
018300*-------------------------------- DB2-AREAS                               
018400     EXEC SQL                                                             
018500          INCLUDE SQLCA                                                   
018600     END-EXEC.                                                            
018700     EXEC SQL                                                             
018800          INCLUDE EXTRACT                                                 
018900     END-EXEC.                                                            
019000*    -COPY EXTRACT -PRE EXTRACT-                                          
019200     EXEC SQL                                                             
019300          INCLUDE EXTUSER                                                 
019400     END-EXEC.                                                            
019500*    -COPY EXTUSER -PRE EXTUSER-                                          
020200     EJECT                                                                
020300*--------------------------------------------------------------           
020400 LINKAGE SECTION.                                                         
020500*--------------------------------------------------------------           
020600 01 PARM.                                                                 
020700     03 PARM-EXTRACTID         PIC X(8).                                  
020800*************************************************************             
020900 PROCEDURE DIVISION USING PARM.                                           
021000*************************************************************             
021100     PERFORM A-INIT                                                       
021200     IF SW-ERROR = NOO                                                    
021210       PERFORM B-CHECK-EXTRACTID                                          
021220       IF SW-ERROR = NOO                                                  
021300         PERFORM C-DISPLAY-PANEL                                          
021400         PERFORM UNTIL SW-ENTER-PRESSED = NOO OR SW-ERROR = YES           
021500                                                                          
021600           IF ISPF-ZZ1LCMD NOT = SPACE                                    
021700             MOVE ISPF-ZZ1LCMD     TO W-ZZLCMD                            
021800             MOVE ISPF-USERID-1    TO W-USERID                            
021900             PERFORM C-LINE-COMMAND                                       
022000           END-IF                                                         
022100                                                                          
022200           PERFORM UNTIL ISPF-ZTDSELS = 0 OR SW-ERROR = YES               
022300             MOVE ISPF-ZZLCMD TO W-ZZLCMD                                 
022400             MOVE ISPF-USERID TO W-USERID                                 
022500             PERFORM C-LINE-COMMAND                                       
022600             IF ISPF-ZTDSELS > 1                                          
022700                CALL 'ISPLINK' USING TBDISPL EXTUSERTAB-NAME              
022800             ELSE                                                         
022900                MOVE ZERO TO ISPF-ZTDSELS                                 
023000             END-IF                                                       
023200           END-PERFORM                                                    
023300           IF SW-ERROR = NOO                                              
023400             PERFORM C-DISPLAY-PANEL                                      
023500           END-IF                                                         
023600         END-PERFORM                                                      
023700       END-IF                                                             
023710     END-IF                                                               
023800     PERFORM Z-FINIT                                                      
023900     GOBACK                                                               
024000     CONTINUE.                                                            
024100                                                                          
024200*--------------------------------------------------------------           
024300 A-INIT SECTION.                                                          
024400*--------------------------------------------------------------           
024500     MOVE 'A-INIT                  ' TO ABEND-SECTION                     
024600D    DISPLAY ABEND-SECTION                                                
024700     SKIP2                                                                
024800     MOVE NOO TO SW-ERROR                                                 
024900     MOVE NOO TO SW-USERID-LISTED                                         
025000     PERFORM AA-INIT-PARMS                                                
025100     PERFORM AB-VDEF-GENERAL                                              
025200     PERFORM AC-VDEF-PANEL-V161700M                                       
025210     IF SW-ERROR = NOO                                                    
025211       PERFORM S01-TBCREATE-ISPFTAB                                       
025220     END-IF                                                               
025500     CONTINUE.                                                            
025600     EJECT                                                                
025700*--------------------------------------------------------------           
025800 AA-INIT-PARMS SECTION.                                                   
025900*--------------------------------------------------------------           
026000     MOVE 'AA-INIT-PARMS           ' TO ABEND-SECTION                     
026100D    DISPLAY ABEND-SECTION                                                
026200     SKIP2                                                                
026300D    DISPLAY 'PARM-EXTRACTID    :' PARM-EXTRACTID                         
026400     IF PARM-EXTRACTID = SPACE                                            
026500        MOVE RCODE-12 TO RCODE                                            
026600        MOVE YES TO SW-ERROR                                              
026700     ELSE                                                                 
026800        MOVE PARM-EXTRACTID TO ISPF-EXTRACTID                             
026900     END-IF                                                               
027000     CONTINUE.                                                            
027100     EJECT                                                                
027200*--------------------------------------------------------------           
027300 AB-VDEF-GENERAL SECTION.                                                 
027400*--------------------------------------------------------------           
027500     MOVE 'AB-VDEF-GENERAL       ' TO ABEND-SECTION                       
027600D    DISPLAY ABEND-SECTION                                                
027700     SKIP2                                                                
027800     CALL 'ISPLINK' USING VDEFINE N-SMSG ISPF-SMSG                        
027900                          CHAR LTH-SMSG                                   
028000     CALL 'ISPLINK' USING VDEFINE N-LMSG ISPF-LMSG                        
028100                          CHAR LTH-LMSG                                   
028200     CALL 'ISPLINK' USING VDEFINE N-ZCMD ISPF-ZCMD                        
028300                          CHAR LTH-ZCMD                                   
028400     CALL 'ISPLINK' USING VDEFINE N-ZTDSELS ISPF-ZTDSELS                  
028500                          CHAR LTH-ZTDSELS                                
028600     CALL 'ISPLINK' USING VDEFINE N-ZWINTTL ISPF-ZWINTTL                  
028700                          CHAR LTH-ZWINTTL                                
028710     CALL 'ISPLINK' USING VDEFINE N-ZZ1LCMD ISPF-ZZ1LCMD                  
028720                          CHAR LTH-ZZ1LCMD                                
028730     CALL 'ISPLINK' USING VDEFINE N-ZZLCMD ISPF-ZZLCMD                    
028740                          CHAR LTH-ZZLCMD                                 
028800     CONTINUE.                                                            
028900     EJECT                                                                
029000*--------------------------------------------------------------           
029100 AC-VDEF-PANEL-V161700M SECTION.                                          
029200*--------------------------------------------------------------           
029300     MOVE 'AC-VDEF-PANEL-V161700M    ' TO ABEND-SECTION                   
029400D    DISPLAY ABEND-SECTION                                                
029500     SKIP2                                                                
029600     CALL 'ISPLINK' USING VDEFINE N-EXTRACTID ISPF-EXTRACTID              
029700                          CHAR LTH-EXTRACTID                              
030200     CALL 'ISPLINK' USING VDEFINE N-USERID-1 ISPF-USERID-1                
030300                          CHAR LTH-USERID-1                               
030400     CALL 'ISPLINK' USING VDEFINE N-USERID ISPF-USERID                    
030500                          CHAR LTH-USERID                                 
030600     CALL 'ISPLINK' USING VDEFINE N-AUTH                                  
030700                          ISPF-AUTH                                       
030800                          CHAR LTH-AUTH                                   
030900     CALL 'ISPLINK' USING VDEFINE N-NAME ISPF-NAME                        
031000                          CHAR LTH-NAME                                   
031100                                                                          
031200     CONTINUE.                                                            
031300     EJECT                                                                
031400*--------------------------------------------------------------           
031500 B-CHECK-EXTRACTID SECTION.                                               
031600*--------------------------------------------------------------           
031700     MOVE 'B-CHECK-EXTRACITD' TO ABEND-SECTION                            
031800D    DISPLAY ABEND-SECTION                                                
031900     SKIP2                                                                
032001     CALL V16194 USING PARM-EXTRACTID                                     
032002     IF RETURN-CODE = 0                                                   
032003        MOVE NOO TO SW-ERROR                                              
032004     ELSE                                                                 
032005       CALL V16193 USING PARM-EXTRACTID                                   
032006       IF RETURN-CODE = 0                                                 
032007          MOVE NOO TO SW-ERROR                                            
032008       ELSE                                                               
032009          MOVE YES TO SW-ERROR                                            
032010          MOVE RCODE-8 TO RCODE                                           
032011       END-IF                                                             
032020     END-IF                                                               
032800     CONTINUE.                                                            
032900     EJECT                                                                
033000*--------------------------------------------------------------           
033100 C-DISPLAY-PANEL SECTION.                                                 
033200*--------------------------------------------------------------           
033300     MOVE 'C-DISPLAY-PANEL          ' TO ABEND-SECTION                    
033400D    DISPLAY ABEND-SECTION                                                
033500     SKIP2                                                                
033600     MOVE SPACE TO ISPF-ZZLCMD                                            
033700     PERFORM SQL-COMMIT                                                   
033800     IF SW-USERID-LISTED = NOO                                            
033900       PERFORM CL-LOAD-EXTUSER                                            
034000     END-IF                                                               
034100     PERFORM S01-TBTOP-ISPFTAB                                            
034200     PERFORM S01-TBDISPL-ISPFTAB                                          
034300     EVALUATE RCODE                                                       
034400       WHEN 0  MOVE YES TO SW-ENTER-PRESSED                               
034500       WHEN 4  MOVE YES TO SW-ENTER-PRESSED                               
034600       WHEN 8  MOVE NOO TO SW-ENTER-PRESSED                               
034700       WHEN OTHER MOVE YES TO SW-ERROR                                    
034800     END-EVALUATE                                                         
034900     IF SW-ENTER-PRESSED = NOO                                            
035000        MOVE RCODE-0 TO RCODE                                             
035100     END-IF                                                               
035200     CONTINUE.                                                            
035300     EJECT                                                                
035400*--------------------------------------------------------------           
035500 C-LINE-COMMAND SECTION.                                                  
035600*--------------------------------------------------------------           
035700     MOVE 'C-LINE-COMMAND          ' TO ABEND-SECTION                     
035800D    DISPLAY ABEND-SECTION                                                
035900     SKIP2                                                                
035910D    DISPLAY 'W-ZZLCMD :' W-ZZLCMD                                        
036000     IF W-ZZLCMD = 'S' OR '/'                                             
036100        PERFORM CS-POPUP                                                  
036200     END-IF                                                               
036300     IF SW-ERROR = NOO                                                    
036400       CALL 'ISPLINK' USING CONTROLL DISPLAYE SAVEE                       
036500       EVALUATE W-ZZLCMD                                                  
036600       WHEN ' ' CONTINUE                                                  
036700       WHEN 'N' PERFORM CC-CREATE-EXTUSER                                 
036800       WHEN 'U' PERFORM CU-UPDATE-EXTUSER                                 
036900       WHEN 'D' PERFORM CD-DELETE-EXTUSER                                 
037000       WHEN OTHER                                                         
037100          MOVE MSG-VIOS101H  TO ISPF-MSG-ID                               
037300       END-EVALUATE                                                       
037400       CALL 'ISPLINK' USING CONTROLL DISPLAYE RESTOREE                    
037500     END-IF                                                               
037600     CONTINUE.                                                            
037700     EJECT                                                                
037800*--------------------------------------------------------------           
037900 CL-LOAD-EXTUSER SECTION.                                                 
038000*--------------------------------------------------------------           
038100     MOVE 'CL-LOAD-EXTUSER       ' TO ABEND-SECTION                       
038200D    DISPLAY ABEND-SECTION                                                
038300     SKIP2                                                                
038400     PERFORM S01-TBCREATE-ISPFTAB                                         
038500     IF SW-ERROR = NOO                                                    
038600        PERFORM SQL-DECLARE-OPEN-EXTUSER                                  
038700        IF SQLCODE = +000                                                 
038800           PERFORM SQL-FETCH-EXTUSER                                      
038900           IF SQLCODE = +100                                              
039000              MOVE MSG-VIOS101O  TO ISPF-MSG-ID                           
039100           END-IF                                                         
039200           PERFORM UNTIL SQLCODE NOT = +000                               
039310             MOVE EXTUSER-USERID  TO V16197-USERID                        
039320             CALL V16197 USING V16197-USERID                              
039330                               V16197-NAME                                
039340             IF RETURN-CODE = 0 THEN                                      
039350               MOVE V16197-NAME      TO ISPF-NAME                         
039360             ELSE                                                         
039370               MOVE 'NAME NOT FOUND' TO ISPF-NAME                         
039380             END-IF                                                       
039900             MOVE EXTUSER-USERID        TO ISPF-USERID                    
040000             MOVE EXTUSER-AUTH          TO ISPF-AUTH                      
040100             PERFORM S01-TBADD-ISPFTAB                                    
040200             PERFORM SQL-FETCH-EXTUSER                                    
040300           END-PERFORM                                                    
040400        END-IF                                                            
040500        PERFORM SQL-CLOSE-EXTUSER                                         
040600           MOVE YES TO SW-USERID-LISTED                                   
040700     END-IF                                                               
040800     MOVE SPACE TO ISPF-ZZ1LCMD, ISPF-USERID-1                            
040900     CONTINUE.                                                            
041000     EJECT                                                                
041100*--------------------------------------------------------------           
041200 CC-CREATE-EXTUSER SECTION.                                               
041300*--------------------------------------------------------------           
041400     MOVE 'CC-CREATE-EXTUSER     ' TO ABEND-SECTION                       
041500D    DISPLAY ABEND-SECTION                                                
041600     SKIP2                                                                
041900     CALL V161702P USING ISPF-EXTRACTID W-USERID                          
042000     EVALUATE RETURN-CODE                                                 
042100       WHEN 0  PERFORM CCA-REFRESH-BILD                                   
042200               MOVE SPACE TO  ISPF-ZZ1LCMD ISPF-USERID-1                  
042300               MOVE MSG-VIOS101A  TO ISPF-MSG-ID                          
042400       WHEN 4  MOVE SPACE TO  ISPF-ZZ1LCMD ISPF-USERID-1                  
042500               MOVE MSG-VIOS101B  TO ISPF-MSG-ID                          
042600       WHEN 8  MOVE MSG-VIOS101L  TO ISPF-MSG-ID                          
042700       WHEN 12 MOVE MSG-VIOS101K  TO ISPF-MSG-ID                          
042800       WHEN OTHER MOVE YES TO SW-ERROR                                    
042900     END-EVALUATE                                                         
043000     CONTINUE.                                                            
043100     EJECT                                                                
043200*--------------------------------------------------------------           
043300 CCA-REFRESH-BILD SECTION.                                                
043400*--------------------------------------------------------------           
043500     MOVE 'CCA-REFRESH-BILD        ' TO ABEND-SECTION                     
043600D    DISPLAY ABEND-SECTION                                                
043700     SKIP2                                                                
043800     PERFORM SQL-SELECT-EXTUSER                                           
043900     IF SQLCODE = +000                                                    
043910       MOVE W-USERID              TO V16197-USERID, ISPF-USERID           
043911       MOVE EXTUSER-AUTH          TO ISPF-AUTH                            
043913       CALL V16197 USING V16197-USERID                                    
043914                         V16197-NAME                                      
043915       IF RETURN-CODE = 0 THEN                                            
043916         MOVE V16197-NAME      TO ISPF-NAME                               
043917       ELSE                                                               
043918         MOVE 'NAME NOT FOUND' TO ISPF-NAME                               
043919       END-IF                                                             
044200       PERFORM S01-TBADD-ISPFTAB                                          
044300     ELSE                                                                 
044400       MOVE YES TO SW-ERROR                                               
044500     END-IF                                                               
044600     CONTINUE.                                                            
044700     EJECT                                                                
044800*--------------------------------------------------------------           
044900 CU-UPDATE-EXTUSER SECTION.                                               
045000*--------------------------------------------------------------           
045100     MOVE 'CU-UPDATE-EXTUSER     ' TO ABEND-SECTION                       
045200D    DISPLAY ABEND-SECTION                                                
045300     SKIP2                                                                
045400     CALL V161703P USING ISPF-EXTRACTID W-USERID                          
045500     EVALUATE RETURN-CODE                                                 
045600       WHEN 0  PERFORM CUA-REFRESH-BILD                                   
045700               MOVE SPACE TO ISPF-ZZ1LCMD ISPF-USERID-1                   
045800               MOVE MSG-VIOS101A TO ISPF-MSG-ID                           
045900       WHEN 4  MOVE SPACE TO ISPF-ZZ1LCMD ISPF-USERID-1                   
046000               MOVE MSG-VIOS101B  TO ISPF-MSG-ID                          
046100       WHEN 8  MOVE MSG-VIOS101M  TO ISPF-MSG-ID                          
046200       WHEN 12 MOVE MSG-VIOS101K  TO ISPF-MSG-ID                          
046300       WHEN OTHER MOVE YES TO SW-ERROR                                    
046400     END-EVALUATE                                                         
046500     CONTINUE.                                                            
046600     EJECT                                                                
046700*--------------------------------------------------------------           
046800 CUA-REFRESH-BILD SECTION.                                                
046900*--------------------------------------------------------------           
047000     MOVE 'CUA-REFRESH-BILD        ' TO ABEND-SECTION                     
047100D    DISPLAY ABEND-SECTION                                                
047200     SKIP2                                                                
047300     PERFORM SQL-SELECT-EXTUSER                                           
047400     IF SQLCODE = +000                                                    
047500       MOVE W-USERID              TO ISPF-USERID                          
047600       PERFORM S01-TBGET-ISPFTAB                                          
047700       IF RCODE = 0                                                       
047800         MOVE EXTUSER-AUTH          TO ISPF-AUTH                          
047900         PERFORM S01-TBMOD-ISPFTAB                                        
048000       END-IF                                                             
048100     ELSE                                                                 
048200       MOVE YES TO SW-ERROR                                               
048300     END-IF                                                               
048400     CONTINUE.                                                            
048500     EJECT                                                                
048600*--------------------------------------------------------------           
048700 CD-DELETE-EXTUSER SECTION.                                               
048800*--------------------------------------------------------------           
048900     MOVE 'CD-DELETE-EXTUSER     ' TO ABEND-SECTION                       
049000D    DISPLAY ABEND-SECTION                                                
049100     SKIP2                                                                
049200     CALL V161705P USING ISPF-EXTRACTID W-USERID                          
049300     EVALUATE RETURN-CODE                                                 
049400       WHEN 0  PERFORM CDA-REFRESH-BILD                                   
049500               MOVE SPACE TO  ISPF-ZZ1LCMD ISPF-USERID-1                  
049600               MOVE MSG-VIOS101A  TO ISPF-MSG-ID                          
049700       WHEN 4  MOVE SPACE TO  ISPF-ZZ1LCMD ISPF-USERID-1                  
049800               MOVE MSG-VIOS101B  TO ISPF-MSG-ID                          
049900       WHEN 8  MOVE MSG-VIOS101N  TO ISPF-MSG-ID                          
050000       WHEN 12 MOVE MSG-VIOS101K  TO ISPF-MSG-ID                          
050100       WHEN OTHER MOVE YES TO SW-ERROR                                    
050200     END-EVALUATE                                                         
050300     CONTINUE.                                                            
050400     EJECT                                                                
050500*--------------------------------------------------------------           
050600 CDA-REFRESH-BILD SECTION.                                                
050700*--------------------------------------------------------------           
050800     MOVE 'CDA-REFRESH-BILD        ' TO ABEND-SECTION                     
050900D    DISPLAY ABEND-SECTION                                                
051000     SKIP2                                                                
051100       MOVE W-USERID      TO ISPF-USERID                                  
051200       PERFORM S01-TBDELETE-ISPFTAB                                       
051300     CONTINUE.                                                            
051400     EJECT                                                                
051500*--------------------------------------------------------------           
051600 CS-POPUP SECTION.                                                        
051700*--------------------------------------------------------------           
051800     MOVE 'CS-POPUP            ' TO ABEND-SECTION                         
051900D    DISPLAY ABEND-SECTION                                                
052000     SKIP2                                                                
052100     MOVE 'ACTIONS FOR AUTHORITY' TO ISPF-ZWINTTL                         
052200     PERFORM S01-ADDPOP                                                   
052300     PERFORM CSA-DISPLAY-POPUP                                            
052400     PERFORM UNTIL SW-ENTER-PRESSED = NOO OR                              
052500                   ISPF-ZZLCMD NOT = SPACE                                
052600                   OR SW-ERROR = YES                                      
052700         PERFORM CSA-DISPLAY-POPUP                                        
052800     END-PERFORM                                                          
052900     PERFORM S01-REMPOP                                                   
053000     MOVE ISPF-ZZLCMD TO W-ZZLCMD                                         
053100     IF SW-ENTER-PRESSED = NOO                                            
053200        MOVE SPACE TO ISPF-ZZ1LCMD, W-ZZLCMD                              
053300        MOVE MSG-VIOS101B  TO ISPF-MSG-ID                                 
053400     END-IF                                                               
053500     CONTINUE.                                                            
053600     EJECT                                                                
053700*--------------------------------------------------------------           
053800 CSA-DISPLAY-POPUP SECTION.                                               
053900*--------------------------------------------------------------           
054000     MOVE 'CSA-DISPLAY-POPUP           ' TO ABEND-SECTION                 
054100D    DISPLAY ABEND-SECTION                                                
054200     SKIP2                                                                
054300     MOVE V1617001 TO ISPF-PANEL                                          
054400     PERFORM S01-DISPLAY-PANEL                                            
054500     IF RETURN-CODE <= 4                                                  
054600        MOVE YES TO SW-ENTER-PRESSED                                      
054700     ELSE                                                                 
054800        MOVE NOO TO SW-ENTER-PRESSED                                      
054900        IF RETURN-CODE > 8                                                
055000           MOVE YES TO SW-ERROR                                           
055100        END-IF                                                            
055200     END-IF                                                               
055300     CONTINUE.                                                            
055400     EJECT                                                                
055500*--------------------------------------------------------------           
055600 Z-FINIT SECTION.                                                         
055700*--------------------------------------------------------------           
055800     MOVE 'Z-FINIT                  ' TO ABEND-SECTION                    
055900*    DISPLAY ABEND-SECTION                                                
056000     SKIP2                                                                
056100     PERFORM ZA-VDEL-GENERAL                                              
056200     PERFORM ZB-VDEL-PANEL-V161700M                                       
056300     MOVE RCODE TO RETURN-CODE                                            
056400     CONTINUE.                                                            
056500     EJECT                                                                
056600*--------------------------------------------------------------           
056700 ZA-VDEL-GENERAL SECTION.                                                 
056800*--------------------------------------------------------------           
056900     MOVE 'ZA-VDEL-GENERAL       ' TO ABEND-SECTION                       
057000D    DISPLAY ABEND-SECTION                                                
057100     SKIP2                                                                
057200     CALL 'ISPLINK' USING VDELETE N-SMSG                                  
057300     CALL 'ISPLINK' USING VDELETE N-LMSG                                  
057400     CALL 'ISPLINK' USING VDELETE N-ZCMD                                  
057500     CALL 'ISPLINK' USING VDELETE N-ZWINTTL                               
057510     CALL 'ISPLINK' USING VDELETE N-ZZ1LCMD                               
057520     CALL 'ISPLINK' USING VDELETE N-ZZLCMD                                
057600     CONTINUE.                                                            
057700     EJECT                                                                
057800*--------------------------------------------------------------           
057900 ZB-VDEL-PANEL-V161700M SECTION.                                          
058000*--------------------------------------------------------------           
058100     MOVE 'ZB-VDEL-PANEL-V161700M  ' TO ABEND-SECTION                     
058200D    DISPLAY ABEND-SECTION                                                
058300     SKIP2                                                                
058500     CALL 'ISPLINK' USING VDELETE N-EXTRACTID                             
058600     CALL 'ISPLINK' USING VDELETE N-USERID                                
058700     CALL 'ISPLINK' USING VDELETE N-USERID-1                              
058800     CALL 'ISPLINK' USING VDELETE N-AUTH                                  
058900     CALL 'ISPLINK' USING VDELETE N-NAME                                  
059000     CONTINUE.                                                            
059100     EJECT                                                                
059200*--------------------------------------------------------------           
059300 S01-DISPLAY-PANEL SECTION.                                               
059400*--------------------------------------------------------------           
059500     MOVE 'S01-DISPLAY-PANEL           ' TO ABEND-SECTION                 
059600D    DISPLAY ABEND-SECTION                                                
059700     SKIP2                                                                
059800     CALL 'ISPLINK' USING DISPLAYE ISPF-PANEL ISPF-MSG-ID                 
059900     MOVE RETURN-CODE TO RCODE RCODE-DISPL                                
060000     MOVE SPACE TO ISPF-MSG-ID                                            
060100     IF RCODE > +8                                                        
060200        PERFORM S99-ERROR-ROUTINE                                         
060300        MOVE YES TO SW-ERROR                                              
060400        MOVE RCODE-16 TO RCODE                                            
060500     END-IF                                                               
060600     CONTINUE.                                                            
060700     EJECT                                                                
060800*--------------------------------------------------------------           
060900 S01-TBCREATE-ISPFTAB SECTION.                                            
061000*--------------------------------------------------------------           
061100     MOVE 'S01-TBCREATE                ' TO ABEND-SECTION                 
061200D    DISPLAY ABEND-SECTION                                                
061300     SKIP2                                                                
061400     CALL 'ISPLINK' USING TBCREATE                                        
061500                          EXTUSERTAB-NAME                                 
061600                          EXTUSERTAB-KEY                                  
061700                          EXTUSERTAB-VAR                                  
061800                          TABNOW                                          
061900                          TABREP                                          
062000     MOVE RETURN-CODE TO RCODE RCODE-DISPL                                
062100     IF RCODE <= +8                                                       
062200        MOVE NOO TO SW-ERROR                                              
062300     ELSE                                                                 
062400        MOVE YES TO SW-ERROR                                              
062500        PERFORM S99-ERROR-ROUTINE                                         
062600        MOVE RCODE-16 TO RCODE                                            
062700     END-IF                                                               
062800     CONTINUE.                                                            
062900     EJECT                                                                
063000*--------------------------------------------------------------           
063100 S01-TBDISPL-ISPFTAB SECTION.                                             
063200*--------------------------------------------------------------           
063300     MOVE 'S01-TBDISPL-ISPFTAB         ' TO ABEND-SECTION                 
063400D    DISPLAY ABEND-SECTION                                                
063500     SKIP2                                                                
063600     CALL 'ISPLINK' USING TBDISPL EXTUSERTAB-NAME V161700M                
063700                          ISPF-MSG-ID                                     
063800     MOVE RETURN-CODE TO RCODE RCODE-DISPL                                
063900     MOVE SPACE TO ISPF-MSG-ID                                            
064000     IF RCODE <= +8                                                       
064100        MOVE NOO TO SW-ERROR                                              
064200     ELSE                                                                 
064300        MOVE YES TO SW-ERROR                                              
064400        PERFORM S99-ERROR-ROUTINE                                         
064500        MOVE RCODE-16 TO RCODE                                            
064600     END-IF                                                               
064700     CONTINUE.                                                            
064800     EJECT                                                                
064900*--------------------------------------------------------------           
065000 S01-TBADD-ISPFTAB SECTION.                                               
065100*--------------------------------------------------------------           
065200     MOVE 'S01-TBADD-ISPFTAB           ' TO ABEND-SECTION                 
065300D    DISPLAY ABEND-SECTION                                                
065400     SKIP2                                                                
065500     CALL 'ISPLINK' USING TBADD                                           
065600                          EXTUSERTAB-NAME                                 
065700     MOVE RETURN-CODE TO RCODE RCODE-DISPL                                
065800     IF RCODE = +0                                                        
065900        MOVE NOO TO SW-ERROR                                              
066000     ELSE                                                                 
066100        MOVE YES TO SW-ERROR                                              
066200        PERFORM S99-ERROR-ROUTINE                                         
066300        MOVE RCODE-16 TO RCODE                                            
066400     END-IF                                                               
066500     CONTINUE.                                                            
066600     EJECT                                                                
066700*--------------------------------------------------------------           
066800 S01-TBMOD-ISPFTAB SECTION.                                               
066900*--------------------------------------------------------------           
067000     MOVE 'S01-TBMOD-ISPFTAB           ' TO ABEND-SECTION                 
067100D    DISPLAY ABEND-SECTION                                                
067200     SKIP2                                                                
067300     CALL 'ISPLINK' USING TBMOD                                           
067400                          EXTUSERTAB-NAME                                 
067500     MOVE RETURN-CODE TO RCODE RCODE-DISPL                                
067600     IF RCODE = +0                                                        
067700        MOVE NOO TO SW-ERROR                                              
067800     ELSE                                                                 
067900        MOVE YES TO SW-ERROR                                              
068000        PERFORM S99-ERROR-ROUTINE                                         
068100        MOVE RCODE-16 TO RCODE                                            
068200     END-IF                                                               
068300     CONTINUE.                                                            
068400     EJECT                                                                
068500*--------------------------------------------------------------           
068600 S01-TBGET-ISPFTAB SECTION.                                               
068700*--------------------------------------------------------------           
068800     MOVE 'S01-TBGET-ISPFTAB           ' TO ABEND-SECTION                 
068900D    DISPLAY ABEND-SECTION                                                
069000     SKIP2                                                                
069100     CALL 'ISPLINK' USING TBGET                                           
069200                          EXTUSERTAB-NAME                                 
069300     MOVE RETURN-CODE TO RCODE RCODE-DISPL                                
069400     IF RCODE <= +8                                                       
069500        MOVE NOO TO SW-ERROR                                              
069600     ELSE                                                                 
069700        MOVE YES TO SW-ERROR                                              
069800        PERFORM S99-ERROR-ROUTINE                                         
069900        MOVE RCODE-16 TO RCODE                                            
070000     END-IF                                                               
070100     CONTINUE.                                                            
070200     EJECT                                                                
070300*--------------------------------------------------------------           
070400 S01-TBDELETE-ISPFTAB SECTION.                                            
070500*--------------------------------------------------------------           
070600     MOVE 'S01-TBDELETE-ISPFTAB        ' TO ABEND-SECTION                 
070700D    DISPLAY ABEND-SECTION                                                
070800     SKIP2                                                                
070900     CALL 'ISPLINK' USING TBDELETE                                        
071000                          EXTUSERTAB-NAME                                 
071100     MOVE RETURN-CODE TO RCODE RCODE-DISPL                                
071200     IF RCODE <= +8                                                       
071300        MOVE NOO TO SW-ERROR                                              
071400     ELSE                                                                 
071500        MOVE YES TO SW-ERROR                                              
071600        PERFORM S99-ERROR-ROUTINE                                         
071700        MOVE RCODE-16 TO RCODE                                            
071800     END-IF                                                               
071900     CONTINUE.                                                            
072000     EJECT                                                                
072100*--------------------------------------------------------------           
072200 S01-TBTOP-ISPFTAB SECTION.                                               
072300*--------------------------------------------------------------           
072400     MOVE 'S01-TBTOP-ISPFTAB           ' TO ABEND-SECTION                 
072500D    DISPLAY ABEND-SECTION                                                
072600     SKIP2                                                                
072700     CALL 'ISPLINK' USING TBTOP                                           
072800                          EXTUSERTAB-NAME                                 
072900     MOVE RETURN-CODE TO RCODE RCODE-DISPL                                
073000     IF RCODE = +0                                                        
073100        MOVE NOO TO SW-ERROR                                              
073200     ELSE                                                                 
073300        MOVE YES TO SW-ERROR                                              
073400        PERFORM S99-ERROR-ROUTINE                                         
073500        MOVE RCODE-16 TO RCODE                                            
073600     END-IF                                                               
073700     CONTINUE.                                                            
073800     EJECT                                                                
073900*--------------------------------------------------------------           
074000 S01-ADDPOP SECTION.                                                      
074100*--------------------------------------------------------------           
074200     MOVE 'S01-ADDPOP                  ' TO ABEND-SECTION                 
074300D    DISPLAY ABEND-SECTION                                                
074400     SKIP2                                                                
074500     CALL 'ISPLINK' USING ADDPOP W-EMPTY-PANEL                            
074600                                 ISPF-ROW-POS ISPF-COL-POS                
074700     MOVE RETURN-CODE TO RCODE RCODE-DISPL                                
074800     IF RCODE = +0                                                        
074900        MOVE NOO TO SW-ERROR                                              
075000     ELSE                                                                 
075100        MOVE YES TO SW-ERROR                                              
075200        PERFORM S99-ERROR-ROUTINE                                         
075300        MOVE RCODE-16 TO RCODE                                            
075400     END-IF                                                               
075500     CONTINUE.                                                            
075600     EJECT                                                                
075700*--------------------------------------------------------------           
075800 S01-REMPOP SECTION.                                                      
075900*--------------------------------------------------------------           
076000     MOVE 'S01-REMPOP                  ' TO ABEND-SECTION                 
076100D    DISPLAY ABEND-SECTION                                                
076200     SKIP2                                                                
076300     CALL 'ISPLINK' USING REMPOP                                          
076400     MOVE RETURN-CODE TO RCODE RCODE-DISPL                                
076500     IF RCODE = +0                                                        
076600        MOVE NOO TO SW-ERROR                                              
076700     ELSE                                                                 
076800        MOVE YES TO SW-ERROR                                              
076900        PERFORM S99-ERROR-ROUTINE                                         
077000        MOVE RCODE-16 TO RCODE                                            
077100     END-IF                                                               
077200     CONTINUE.                                                            
077300     EJECT                                                                
077400*--------------------------------------------------------------           
077500 S99-ERROR-ROUTINE SECTION.                                               
077600*--------------------------------------------------------------           
077700     SKIP2                                                                
077800     STRING 'ERROR IN ' ABEND-SECTION ' RCODE ='                          
077900     RCODE-DISPL DELIMITED BY SIZE                                        
078000     INTO W-ERROR-MESSAGE                                                 
078100     DISPLAY W-ERROR-MESSAGE                                              
078200     CONTINUE.                                                            
078300     EJECT                                                                
079900*--------------------------------------------------------------           
080000 SQL-SELECT-EXTUSER SECTION.                                              
080100*--------------------------------------------------------------           
080200     MOVE 'SQL-SELECT-EXTUSER          ' TO ABEND-SECTION                 
080300D    DISPLAY ABEND-SECTION                                                
080400     SKIP2                                                                
080500     EXEC SQL                                                             
080600        SELECT AUTH                                                       
080700        INTO :EXTUSER-AUTH                                                
080800        FROM EXTRACT_USER                                                 
080900        WHERE EXTRACTID = :ISPF-EXTRACTID                                 
081000        AND   USERID    = :W-USERID                                       
081100     END-EXEC                                                             
081200     PERFORM S95-CONTROL-SQLCODE                                          
081300     CONTINUE.                                                            
081400     EJECT                                                                
081500*--------------------------------------------------------------           
081600 SQL-DECLARE-OPEN-EXTUSER SECTION.                                        
081700*--------------------------------------------------------------           
081800     MOVE 'SQL-DECLARE-OPEN-EXTUSER    ' TO ABEND-SECTION                 
081900D    DISPLAY ABEND-SECTION                                                
082000     SKIP2                                                                
082100     EXEC SQL                                                             
082200         DECLARE CRS-EXTUSER-1 CURSOR FOR                                 
082300         SELECT USERID, AUTH                                              
082400         FROM EXTRACT_USER                                                
082500         WHERE EXTRACTID = :ISPF-EXTRACTID                                
082600     END-EXEC                                                             
082700     EXEC SQL                                                             
082800         OPEN CRS-EXTUSER-1                                               
082900     END-EXEC                                                             
083000     PERFORM S95-CONTROL-SQLCODE                                          
083100     CONTINUE.                                                            
083200     EJECT                                                                
083300*--------------------------------------------------------------           
083400 SQL-FETCH-EXTUSER SECTION.                                               
083500*--------------------------------------------------------------           
083600     MOVE 'SQL-FETCH-EXTUSER           ' TO ABEND-SECTION                 
083700D    DISPLAY ABEND-SECTION                                                
083800     SKIP2                                                                
083900     EXEC SQL                                                             
084000         FETCH CRS-EXTUSER-1 INTO :EXTUSER-USERID,                        
084100                                  :EXTUSER-AUTH                           
084200     END-EXEC                                                             
084300     PERFORM S95-CONTROL-SQLCODE                                          
084400     CONTINUE.                                                            
084500     EJECT                                                                
084600*--------------------------------------------------------------           
084700 SQL-CLOSE-EXTUSER SECTION.                                               
084800*--------------------------------------------------------------           
084900     MOVE 'SQL-CLOSE-EXTUSER           ' TO ABEND-SECTION                 
085000D    DISPLAY ABEND-SECTION                                                
085100     SKIP2                                                                
085200     EXEC SQL                                                             
085300         CLOSE CRS-EXTUSER-1                                              
085400     END-EXEC                                                             
085500     PERFORM S95-CONTROL-SQLCODE                                          
085600     CONTINUE.                                                            
085700     EJECT                                                                
085800*--------------------------------------------------------------           
085900 SQL-COMMIT SECTION.                                                      
086000*--------------------------------------------------------------           
086100     MOVE 'SQL-COMMIT                  ' TO ABEND-SECTION                 
086200D    DISPLAY ABEND-SECTION                                                
086300     SKIP2                                                                
086400     EXEC SQL                                                             
086500         COMMIT                                                           
086600     END-EXEC                                                             
086700     PERFORM S95-CONTROL-SQLCODE                                          
086800     CONTINUE.                                                            
086900     EJECT                                                                
087000*-------------------------------- DB2 FELHANTERING SEKTION                
087100*    -COPY V161PS                                                         
087200*++INCLUDE V161PS                                                         
