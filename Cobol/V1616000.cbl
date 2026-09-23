000100***************************************************************           
000200 ID  DIVISION.                                                            
000300***************************************************************           
000400 PROGRAM-ID.    V1616000.                                                 
000500 AUTHOR.        CARINA HOLMQVIST                                          
000600 DATE-WRITTEN.  SEPTEMBER 1991                                            
000700***************************************************************           
000800*                                                                         
000900*REMARKS:                                                                 
001000*                                                                         
001100*  * MAIN ADM VIOSE                                                       
001200*                                                                         
001300*  * DB2-TABLES:                                                          
001400*        EXTRACT                                                          
001500*                                                                         
001600*  * SUBPROGRAMS:                                                         
001700*        V16162 V16163 V16164 V16165                                      
001800*        V16170 V16193 V16194                                             
001900*                                                                         
002000*  * RETURNCODES:                                                         
002100*                                                                         
002200*        16 - SEVERE FILE-ERROR                                           
002300*        20 - SEVERE DB2-ERROR                                            
002400*                                                                         
002500***************************************************************           
002600     EJECT                                                                
002700***************************************************************           
002800 ENVIRONMENT DIVISION.                                                    
002900***************************************************************           
003000     SKIP2                                                                
003100*--------------------------------------------------------------           
003200 CONFIGURATION SECTION.                                                   
003300*--------------------------------------------------------------           
003400 SOURCE-COMPUTER. IBM-370.                                                
003500*SOURCE-COMPUTER. IBM-370 WITH DEBUGGING MODE.                            
003600     SKIP2                                                                
003700***************************************************************           
003800 DATA DIVISION.                                                           
003900***************************************************************           
004000     SKIP2                                                                
004100*--------------------------------------------------------------           
004200 WORKING-STORAGE SECTION.                                                 
004300*--------------------------------------------------------------           
004400 01  PROGRAM-NAME                 PIC X(8)  VALUE 'V1616000'.             
004500 01  ABEND-SECTION                PIC X(25) VALUE SPACE.                  
004600 01  RETURN-CODES.                                                        
004700     03  RCODE                    PIC S9(4) COMP SYNC VALUE ZERO.         
004800     03  RCODE-4                  PIC S9(4) COMP SYNC VALUE 4.            
004810     03  RCODE-7                  PIC S9(4) COMP SYNC VALUE 7.            
004900     03  RCODE-8                  PIC S9(4) COMP SYNC VALUE 8.            
005000     03  RCODE-16                 PIC S9(4) COMP SYNC VALUE 16.           
005100     03  RCODE-20                 PIC S9(4) COMP SYNC VALUE 20.           
005200     03  RCODE-DISPL              PIC Z(4)-.                              
005300     SKIP2                                                                
005400 01  GENERAL-CONSTANTS.                                                   
005500     03 YES                       PIC X(1)  VALUE 'Y'.                    
005600     03 NOO                       PIC X(1)  VALUE 'N'.                    
005610     03 AAA                       PIC X(1)  VALUE 'A'.                    
005700     SKIP2                                                                
005800 01  SWITCHES.                                                            
005900     03 SW-ENTER-PRESSED          PIC X(1)  VALUE 'N'.                    
006000     03 SW-ERROR                  PIC X(1)  VALUE 'N'.                    
006100     03 SW-EXTRACT-LISTED         PIC X(1)  VALUE 'N'.                    
006200     03 SW-EQUAL                  PIC X(1)  VALUE 'N'.                    
006300     SKIP2                                                                
006400 01  W-AREAS.                                                             
006500     03 W-ERROR-MESSAGE           PIC X(40) VALUE SPACE.                  
006600     03 W-EXTRACTID               PIC X(8)  VALUE SPACE.                  
006700     03 W-ZZLCMD                  PIC X(1)  VALUE SPACE.                  
006800     03 W-EMPTY-PANEL             PIC X(8)  VALUE SPACE.                  
006900     03 W-COUNTER                 PIC S9(6) VALUE ZERO COMP.              
006910     03 W-NEW-EXTRACT             PIC X(8)  VALUE SPACE.                  
006920     03 W-DESCR                   PIC X(25) VALUE SPACE.                  
007000     SKIP2                                                                
007100 01  N-GENERAL.                                                           
007200     03 N-SMSG                    PIC X(8)  VALUE 'SMSG    '.             
007300     03 N-LMSG                    PIC X(8)  VALUE 'LMSG    '.             
007400     03 N-ZCMD                    PIC X(8)  VALUE 'ZCMD    '.             
007500     03 N-ZTDSELS                 PIC X(8)  VALUE 'ZTDSELS '.             
007600     03 N-ZWINTTL                 PIC X(8)  VALUE 'ZWINTTL '.             
007700     03 N-ZZ1LCMD                 PIC X(8)  VALUE 'ZZ1LCMD '.             
007800     03 N-ZZLCMD                  PIC X(8)  VALUE 'ZZLCMD  '.             
007900     SKIP2                                                                
008000 01  N-PANEL-V161600M.                                                    
008100     03 N-EXTRACTID-1             PIC X(8)  VALUE 'V161600A'.             
008200     03 N-EXTRACTID               PIC X(8)  VALUE 'V161600B'.             
008300     03 N-DESCRIPTION             PIC X(8)  VALUE 'V161600C'.             
008400     03 N-OWNER                   PIC X(8)  VALUE 'V161600D'.             
008500     03 N-RESTRICTED              PIC X(8)  VALUE 'V161600E'.             
008600     SKIP2                                                                
008700 01  LTH-GENERAL.                                                         
008800     03 LTH-SMSG                  PIC S9(6) VALUE 24 COMP.                
008900     03 LTH-LMSG                  PIC S9(6) VALUE 7  COMP.                
009000     03 LTH-ZCMD                  PIC S9(6) VALUE 50 COMP.                
009100     03 LTH-ZTDSELS               PIC S9(6) VALUE 4  COMP.                
009200     03 LTH-ZWINTTL               PIC S9(6) VALUE 30 COMP.                
009300     03 LTH-ZZLCMD                PIC S9(6) VALUE 1  COMP.                
009400     03 LTH-ZZ1LCMD               PIC S9(6) VALUE 1  COMP.                
009500     SKIP2                                                                
009600 01  LTH-PANEL-V161600M.                                                  
009700     03 LTH-EXTRACTID-1           PIC S9(6) VALUE 8  COMP.                
009800     03 LTH-EXTRACTID             PIC S9(6) VALUE 8  COMP.                
009900     03 LTH-DESCRIPTION           PIC S9(6) VALUE 25 COMP.                
010000     03 LTH-OWNER                 PIC S9(6) VALUE 7  COMP.                
010100     03 LTH-RESTRICTED            PIC S9(6) VALUE 1  COMP.                
010200     SKIP2                                                                
010300 01  ISPF-GENERAL.                                                        
010400     03 ISPF-SMSG                 PIC X(24) VALUE SPACE.                  
010500     03 ISPF-LMSG                 PIC X(74) VALUE SPACE.                  
010600     03 ISPF-ZCMD                 PIC X(50) VALUE SPACE.                  
010700     03 ISPF-ZTDSELS              PIC 9(4)  VALUE ZERO.                   
010800     03 ISPF-ZWINTTL              PIC X(30) VALUE ZERO.                   
010900     03 ISPF-ZZ1LCMD              PIC X(1)  VALUE SPACE.                  
011000     03 ISPF-ZZLCMD               PIC X(1)  VALUE SPACE.                  
011100 01  ISPF-PANEL-V161600M.                                                 
011200     03 ISPF-EXTRACTID-1          PIC X(8)  VALUE SPACE.                  
011300     03 ISPF-EXTRACTID            PIC X(8)  VALUE SPACE.                  
011400     03 ISPF-DESCRIPTION          PIC X(25) VALUE SPACE.                  
011500     03 ISPF-OWNER                PIC X(7)  VALUE SPACE.                  
011600     03 ISPF-RESTRICTED           PIC X(1)  VALUE SPACE.                  
011700 01  DYNAMIC-SUBPROGRAMS.                                                 
011800     03 V161602P                  PIC X(8)  VALUE 'V1616020'.             
011900     03 V161603P                  PIC X(8)  VALUE 'V1616030'.             
012000     03 V161604P                  PIC X(8)  VALUE 'V1616040'.             
012100     03 V161605P                  PIC X(8)  VALUE 'V1616050'.             
012200     03 V16130                    PIC X(8)  VALUE 'V16130  '.             
012210     03 V16170                    PIC X(8)  VALUE 'V16170  '.             
012300     03 V16193                    PIC X(8)  VALUE 'V16193  '.             
012310     03 V16194                    PIC X(8)  VALUE 'V16194  '.             
012400     SKIP2                                                                
012500*--------------------------------------------------------------           
012600*ISPF-CONSTANTS                                                           
012700*--------------------------------------------------------------           
012800 01  DISPLAYE                     PIC X(8)  VALUE 'DISPLAY '.             
012900 01  SELECTT                      PIC X(8)  VALUE 'SELECT  '.             
013000 01  ADDPOP                       PIC X(8)  VALUE 'ADDPOP  '.             
013100 01  REMPOP                       PIC X(8)  VALUE 'REMPOP  '.             
013200 01  CONTROLL                     PIC X(8)  VALUE 'CONTROL '.             
013300 01  SAVEE                        PIC X(8)  VALUE 'SAVE    '.             
013400 01  RESTOREE                     PIC X(8)  VALUE 'RESTORE '.             
013500 01  VDEFINE                      PIC X(8)  VALUE 'VDEFINE '.             
013600 01  VDELETE                      PIC X(8)  VALUE 'VDELETE '.             
013700 01  TBADD                        PIC X(8)  VALUE 'TBADD   '.             
013800 01  TBMOD                        PIC X(8)  VALUE 'TBMOD   '.             
013900 01  TBGET                        PIC X(8)  VALUE 'TBGET   '.             
014000 01  TBDELETE                     PIC X(8)  VALUE 'TBDELETE'.             
014100 01  TBTOP                        PIC X(8)  VALUE 'TBTOP   '.             
014200 01  TBCREATE                     PIC X(8)  VALUE 'TBCREATE'.             
014300 01  TBDISPL                      PIC X(8)  VALUE 'TBDISPL '.             
014400 01  TABNOW                       PIC X(8)  VALUE 'NOWRITE '.             
014500 01  TABREP                       PIC X(8)  VALUE 'REPLACE '.             
014600 01  CHAR                         PIC X(8)  VALUE 'CHAR    '.             
014700*--------------------------------------------------------------           
014800*DEKLARATION AV ISPF-FÄLT                                                 
014900*--------------------------------------------------------------           
015000 01  V161600M                     PIC X(8)   VALUE 'V1616000'.            
015100 01  V1616001                     PIC X(8)   VALUE 'V1616001'.            
015200 01  V1616002                     PIC X(8)   VALUE 'V1616002'.            
015300 01  V1616003                     PIC X(8)   VALUE 'V1616003'.            
015400 01  ISPF-PANEL                   PIC X(8)   VALUE SPACE.                 
015500 01  ISPF-ROW-POS                 PIC S9(6) COMP SYNC VALUE +2.           
015600 01  ISPF-COL-POS                 PIC S9(6) COMP SYNC VALUE +20.          
015700 01  ISPF-MSG-ID                  PIC X(8)   VALUE SPACE.                 
015800     EJECT                                                                
015900 01  EXTRACTTAB.                                                          
016000     03 EXTRACTTAB-NAME           PIC X(8)  VALUE 'V1616000'.             
016100     03 EXTRACTTAB-KEY            PIC X(10)  VALUE                        
016200     '(V161600B)'.                                                        
016300     03 EXTRACTTAB-VAR            PIC X(28) VALUE                         
016400     '(V161600C V161600D V161600E)'.                                      
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
017510     03 MSG-VIOS101P              PIC X(8)  VALUE 'VIOS101P'.             
017520     03 MSG-VIOS101R              PIC X(8)  VALUE 'VIOS101R'.             
017530     03 MSG-VIOS101S              PIC X(8)  VALUE 'VIOS101S'.             
017540     03 MSG-VIOS101T              PIC X(8)  VALUE 'VIOS101T'.             
017550     03 MSG-VIOS101U              PIC X(8)  VALUE 'VIOS101U'.             
017560     03 MSG-VIOS101V              PIC X(8)  VALUE 'VIOS101V'.             
017600*-------------------------------- DB2 ERROR HANDLING                      
017700*    -COPY V161WS                                                         
017800*++INCLUDE V161WS                                                         
017900*-------------------------------- DB2-AREAS                               
018000     EXEC SQL                                                             
018100          INCLUDE SQLCA                                                   
018200     END-EXEC.                                                            
018300     EXEC SQL                                                             
018400          INCLUDE EXTRACT                                                 
018500     END-EXEC.                                                            
018600*    -COPY EXTRACT -PRE EXTRACT-                                          
018700*++INCLUDE EXTRACT                                                        
018800     EJECT                                                                
018900*************************************************************             
019000 PROCEDURE DIVISION.                                                      
019100*************************************************************             
019200     PERFORM A-INIT                                                       
019300     IF SW-ERROR = NOO                                                    
019400       PERFORM B-DISPLAY-PANEL                                            
019500       PERFORM UNTIL SW-ENTER-PRESSED = NOO OR SW-ERROR = YES             
019600                                                                          
019700         IF ISPF-ZZ1LCMD NOT = SPACE                                      
019800           MOVE ISPF-ZZ1LCMD     TO W-ZZLCMD                              
019900           MOVE ISPF-EXTRACTID-1 TO W-EXTRACTID                           
019910           MOVE ISPF-DESCRIPTION TO W-DESCR                               
020000           PERFORM C-LINE-COMMAND                                         
020100         END-IF                                                           
020200                                                                          
020300         PERFORM UNTIL ISPF-ZTDSELS = 0 OR SW-ERROR = YES                 
020400           MOVE ISPF-ZZLCMD TO W-ZZLCMD                                   
020500           MOVE ISPF-EXTRACTID TO W-EXTRACTID                             
020510           MOVE ISPF-DESCRIPTION TO W-DESCR                               
020600           PERFORM C-LINE-COMMAND                                         
020700           IF ISPF-ZTDSELS > 1                                            
020800              CALL 'ISPLINK' USING TBDISPL EXTRACTTAB-NAME                
020900           ELSE                                                           
021000              MOVE ZERO TO ISPF-ZTDSELS                                   
021100           END-IF                                                         
021200                                                                          
021300         END-PERFORM                                                      
021400         IF SW-ERROR = NOO                                                
021500           PERFORM B-DISPLAY-PANEL                                        
021600         END-IF                                                           
021700       END-PERFORM                                                        
021800     END-IF                                                               
021900     PERFORM Z-FINIT                                                      
022000     GOBACK                                                               
022100     CONTINUE.                                                            
022200                                                                          
022300*--------------------------------------------------------------           
022400 A-INIT SECTION.                                                          
022500*--------------------------------------------------------------           
022600     MOVE 'A-INIT                  ' TO ABEND-SECTION                     
022700D    DISPLAY ABEND-SECTION                                                
022800     SKIP2                                                                
022900     MOVE NOO TO SW-ERROR                                                 
023000     MOVE NOO TO SW-EXTRACT-LISTED                                        
023100     PERFORM AA-VDEF-GENERAL                                              
023200     PERFORM AB-VDEF-PANEL-V161600M                                       
023300     PERFORM S01-TBCREATE-ISPFTAB                                         
023400     CONTINUE.                                                            
023500     EJECT                                                                
023600*--------------------------------------------------------------           
023700 AA-VDEF-GENERAL SECTION.                                                 
023800*--------------------------------------------------------------           
023900     MOVE 'AA-VDEF-GENERAL         ' TO ABEND-SECTION                     
024000D    DISPLAY ABEND-SECTION                                                
024100     SKIP2                                                                
024200     CALL 'ISPLINK' USING VDEFINE N-SMSG ISPF-SMSG                        
024300                          CHAR LTH-SMSG                                   
024400     CALL 'ISPLINK' USING VDEFINE N-LMSG ISPF-LMSG                        
024500                          CHAR LTH-LMSG                                   
024600     CALL 'ISPLINK' USING VDEFINE N-ZCMD ISPF-ZCMD                        
024700                          CHAR LTH-ZCMD                                   
024800     CALL 'ISPLINK' USING VDEFINE N-ZTDSELS ISPF-ZTDSELS                  
024900                          CHAR LTH-ZTDSELS                                
025000     CALL 'ISPLINK' USING VDEFINE N-ZWINTTL ISPF-ZWINTTL                  
025100                          CHAR LTH-ZWINTTL                                
025200     CALL 'ISPLINK' USING VDEFINE N-ZZ1LCMD ISPF-ZZ1LCMD                  
025300                          CHAR LTH-ZZ1LCMD                                
025400     CALL 'ISPLINK' USING VDEFINE N-ZZLCMD ISPF-ZZLCMD                    
025500                          CHAR LTH-ZZLCMD                                 
025600     CONTINUE.                                                            
025700     EJECT                                                                
025800*--------------------------------------------------------------           
025900 AB-VDEF-PANEL-V161600M SECTION.                                          
026000*--------------------------------------------------------------           
026100     MOVE 'AB-VDEF-PANEL-V161600M    ' TO ABEND-SECTION                   
026200D    DISPLAY ABEND-SECTION                                                
026300     SKIP2                                                                
026400     CALL 'ISPLINK' USING VDEFINE N-EXTRACTID-1 ISPF-EXTRACTID-1          
026500                          CHAR LTH-EXTRACTID-1                            
026600     CALL 'ISPLINK' USING VDEFINE N-EXTRACTID ISPF-EXTRACTID              
026700                          CHAR LTH-EXTRACTID                              
026800     CALL 'ISPLINK' USING VDEFINE N-DESCRIPTION ISPF-DESCRIPTION          
026900                          CHAR LTH-DESCRIPTION                            
027000     CALL 'ISPLINK' USING VDEFINE N-OWNER       ISPF-OWNER                
027100                          CHAR LTH-OWNER                                  
027200     CALL 'ISPLINK' USING VDEFINE N-RESTRICTED  ISPF-RESTRICTED           
027300                          CHAR LTH-RESTRICTED                             
027400                                                                          
027500     CONTINUE.                                                            
027600     EJECT                                                                
027700*--------------------------------------------------------------           
027800 B-DISPLAY-PANEL SECTION.                                                 
027900*--------------------------------------------------------------           
028000     MOVE 'B-DISPLAY-PANEL          ' TO ABEND-SECTION                    
028100D    DISPLAY ABEND-SECTION                                                
028200     SKIP2                                                                
028300     MOVE SPACE TO ISPF-ZZLCMD                                            
028400     PERFORM SQL-COMMIT                                                   
028500     IF SW-EXTRACT-LISTED = NOO                                           
028600       MOVE V161600M TO ISPF-PANEL                                        
028700       PERFORM S01-DISPLAY-PANEL                                          
028800       EVALUATE RCODE                                                     
028900         WHEN 0     MOVE YES TO SW-ENTER-PRESSED                          
029000         WHEN 8     MOVE NOO TO SW-ENTER-PRESSED                          
029100         WHEN OTHER MOVE YES TO SW-ERROR                                  
029200       END-EVALUATE                                                       
029300     ELSE                                                                 
029400       PERFORM S01-TBTOP-ISPFTAB                                          
029500       PERFORM S01-TBDISPL-ISPFTAB                                        
029600       EVALUATE RCODE                                                     
029700         WHEN 0     MOVE YES TO SW-ENTER-PRESSED                          
029800         WHEN 4     MOVE YES TO SW-ENTER-PRESSED                          
029900         WHEN 8     MOVE NOO TO SW-EXTRACT-LISTED                         
030000                    MOVE V161600M TO ISPF-PANEL                           
030100                    PERFORM S01-DISPLAY-PANEL                             
030200                    EVALUATE RCODE                                        
030300                      WHEN 0  MOVE YES TO SW-ENTER-PRESSED                
030400                      WHEN 8  MOVE NOO TO SW-ENTER-PRESSED                
030500                      WHEN OTHER MOVE YES TO SW-ERROR                     
030600                    END-EVALUATE                                          
030700         WHEN OTHER MOVE YES TO SW-ERROR                                  
030800       END-EVALUATE                                                       
030900     END-IF                                                               
031000     CONTINUE.                                                            
031100     EJECT                                                                
031200*--------------------------------------------------------------           
031300 C-LINE-COMMAND SECTION.                                                  
031400*--------------------------------------------------------------           
031500     MOVE 'C-LINE-COMMAND          ' TO ABEND-SECTION                     
031600D    DISPLAY ABEND-SECTION                                                
031700     SKIP2                                                                
031800D    DISPLAY 'W-ZZLCMD :' W-ZZLCMD                                        
031900     IF W-ZZLCMD = 'S' OR '/'                                             
032000        PERFORM CS-POPUP                                                  
032100     END-IF                                                               
032200     IF SW-ERROR = NOO                                                    
032300       CALL 'ISPLINK' USING CONTROLL DISPLAYE SAVEE                       
032400       EVALUATE W-ZZLCMD                                                  
032500       WHEN ' ' CONTINUE                                                  
032600       WHEN 'A' PERFORM CA-EXTRACT-AUTHORITY                              
032610       WHEN 'C' PERFORM CC-COPY-EXTRACTID                                 
032620       WHEN 'R' PERFORM CC-COPY-EXTRACTID                                 
032700       WHEN 'L' PERFORM CL-LOAD-EXTRACT                                   
032800       WHEN 'N' PERFORM CN-CREATE-EXTRACTID                               
032900       WHEN 'V' PERFORM CV-VIEW-EXTRACTID                                 
033000       WHEN 'U' PERFORM CU-UPDATE-EXTRACTID                               
033100       WHEN 'D' PERFORM CD-DELETE-EXTRACTID                               
033200       WHEN OTHER                                                         
033300          MOVE MSG-VIOS101H  TO ISPF-MSG-ID                               
033400       END-EVALUATE                                                       
033500       CALL 'ISPLINK' USING CONTROLL DISPLAYE RESTOREE                    
033600     END-IF                                                               
033700     CONTINUE.                                                            
033800     EJECT                                                                
033900*--------------------------------------------------------------           
034000 CA-EXTRACT-AUTHORITY SECTION.                                            
034100*--------------------------------------------------------------           
034200     MOVE 'CA-EXTRACT-AUTHORITY    ' TO ABEND-SECTION                     
034300D    DISPLAY ABEND-SECTION                                                
034400     SKIP2                                                                
034500     CALL V16170 USING W-EXTRACTID                                        
034600     EVALUATE RETURN-CODE                                                 
034700       WHEN 0  MOVE SPACE        TO ISPF-ZZ1LCMD ISPF-EXTRACTID-1         
034800               MOVE MSG-VIOS101A TO ISPF-MSG-ID                           
034900       WHEN 4  MOVE SPACE        TO ISPF-ZZ1LCMD ISPF-EXTRACTID-1         
035000               MOVE MSG-VIOS101B TO ISPF-MSG-ID                           
035100       WHEN 8  MOVE MSG-VIOS101J TO ISPF-MSG-ID                           
035200       WHEN 12 MOVE MSG-VIOS101G TO ISPF-MSG-ID                           
035300       WHEN OTHER MOVE YES TO SW-ERROR                                    
035400     END-EVALUATE                                                         
035500     PERFORM AA-VDEF-GENERAL                                              
035600     CONTINUE.                                                            
035610     EJECT                                                                
035620*--------------------------------------------------------------           
035630 CC-COPY-EXTRACTID SECTION.                                               
035640*--------------------------------------------------------------           
035650     MOVE 'CC-COPY-EXTRACTID     ' TO ABEND-SECTION                       
035660D    DISPLAY ABEND-SECTION                                                
035670     SKIP2                                                                
035671     MOVE SPACE     TO W-NEW-EXTRACT                                      
035680     CALL V16130  USING W-ZZLCMD W-EXTRACTID W-DESCR W-NEW-EXTRACT        
035690     EVALUATE RETURN-CODE                                                 
035691       WHEN 0  PERFORM CCA-REFRESH-PANEL                                  
035692               MOVE SPACE TO ISPF-ZZ1LCMD ISPF-EXTRACTID-1                
035693               IF W-ZZLCMD = 'C'                                          
035694                  MOVE MSG-VIOS101R TO ISPF-MSG-ID                        
035695               ELSE                                                       
035696                  MOVE MSG-VIOS101S TO ISPF-MSG-ID                        
035697                  MOVE W-EXTRACTID  TO ISPF-EXTRACTID                     
035698                  PERFORM S01-TBDELETE-ISPFTAB                            
035699               END-IF                                                     
035700       WHEN 4  MOVE SPACE TO ISPF-ZZ1LCMD ISPF-EXTRACTID-1                
035701               MOVE MSG-VIOS101B  TO ISPF-MSG-ID                          
035703       WHEN 7  IF W-ZZLCMD = 'C'                                          
035704                  MOVE MSG-VIOS101P TO ISPF-MSG-ID                        
035705               ELSE                                                       
035706                  MOVE MSG-VIOS101T TO ISPF-MSG-ID                        
035709               END-IF                                                     
035710       WHEN 8  MOVE MSG-VIOS101D  TO ISPF-MSG-ID                          
035711       WHEN 12 MOVE MSG-VIOS101G  TO ISPF-MSG-ID                          
035712       WHEN OTHER MOVE YES TO SW-ERROR                                    
035713     END-EVALUATE                                                         
035714     CONTINUE.                                                            
035715     EJECT                                                                
035716*--------------------------------------------------------------           
035717 CCA-REFRESH-PANEL SECTION.                                               
035718*--------------------------------------------------------------           
035719     MOVE 'CCA-REFRESH-PANEL        ' TO ABEND-SECTION                    
035720D    DISPLAY ABEND-SECTION                                                
035721     SKIP2                                                                
035722     MOVE W-NEW-EXTRACT       TO ISPF-EXTRACTID                           
035723*    MOVE NOO                 TO ISPF-RESTRICTED                          
035724     PERFORM S01-TBADD-ISPFTAB                                            
035725     CONTINUE.                                                            
035730     EJECT                                                                
035800*--------------------------------------------------------------           
035900 CL-LOAD-EXTRACT SECTION.                                                 
036000*--------------------------------------------------------------           
036100     MOVE 'CL-LOAD-EXTRACT       ' TO ABEND-SECTION                       
036200D    DISPLAY ABEND-SECTION                                                
036300     SKIP2                                                                
036400     PERFORM S01-TBCREATE-ISPFTAB                                         
036500     IF SW-ERROR = NOO                                                    
036600        PERFORM CLA-INSPECT-EXTRACTID                                     
036700        PERFORM SQL-DECLARE-OPEN-EXTRACT                                  
036800        IF SQLCODE = +000                                                 
036900           PERFORM SQL-FETCH-EXTRACT                                      
037000           IF SQLCODE = +000                                              
037100              MOVE YES TO SW-EXTRACT-LISTED                               
037200           ELSE                                                           
037300              MOVE MSG-VIOS101I  TO ISPF-MSG-ID                           
037400           END-IF                                                         
037500           PERFORM UNTIL SQLCODE NOT = +000                               
037600             MOVE EXTRACT-EXTRACTID   TO ISPF-EXTRACTID                   
037700             MOVE EXTRACT-DESCRIPTION TO ISPF-DESCRIPTION                 
037800             MOVE EXTRACT-OWNER       TO ISPF-OWNER                       
037900             CALL V16194 USING EXTRACT-EXTRACTID                          
038000             IF RETURN-CODE = 0                                           
038100D              DISPLAY 'V16194 RETURNCODE-0'                              
038200               MOVE NOO                 TO ISPF-RESTRICTED                
038300             ELSE                                                         
038400D              DISPLAY 'V16194 RETURNCODE-ELSE'                           
038410               CALL V16193 USING EXTRACT-EXTRACTID                        
038411               IF RETURN-CODE = 0                                         
038412D                DISPLAY 'V16193 RETURNCODE-0'                            
038413                 MOVE AAA                 TO ISPF-RESTRICTED              
038414               ELSE                                                       
038415D                DISPLAY 'V16193 RETURNCODE-ELSE'                         
038416                 MOVE YES                 TO ISPF-RESTRICTED              
038420               END-IF                                                     
038600             END-IF                                                       
038700             PERFORM S01-TBADD-ISPFTAB                                    
038800             PERFORM SQL-FETCH-EXTRACT                                    
038900           END-PERFORM                                                    
039000        END-IF                                                            
039100        PERFORM SQL-CLOSE-EXTRACT                                         
039200     END-IF                                                               
039300     MOVE SPACE TO ISPF-ZZ1LCMD, ISPF-EXTRACTID-1                         
039400     CONTINUE.                                                            
039500     EJECT                                                                
039600*--------------------------------------------------------------           
039700 CLA-INSPECT-EXTRACTID SECTION.                                           
039800*--------------------------------------------------------------           
039900     MOVE 'CLA-INSPECT-EXTRACTID  ' TO ABEND-SECTION                      
040000D    DISPLAY ABEND-SECTION                                                
040100     SKIP2                                                                
040200     MOVE ZERO TO W-COUNTER                                               
040300     INSPECT W-EXTRACTID TALLYING W-COUNTER FOR ALL '*'                   
040400             REPLACING ALL '*' BY '%'                                     
040500     IF W-COUNTER > 0                                                     
040600        MOVE YES TO SW-EQUAL                                              
040700        INSPECT W-EXTRACTID                                               
040800             REPLACING ALL SPACES BY '%'                                  
040900     ELSE                                                                 
041000        MOVE NOO TO SW-EQUAL                                              
041100     END-IF                                                               
041300     CONTINUE.                                                            
041400     EJECT                                                                
041500*--------------------------------------------------------------           
041600 CN-CREATE-EXTRACTID SECTION.                                             
041700*--------------------------------------------------------------           
041800     MOVE 'CN-CREATE-EXTRACTID     ' TO ABEND-SECTION                     
041900D    DISPLAY ABEND-SECTION                                                
042000     SKIP2                                                                
042100     CALL V161602P USING W-EXTRACTID                                      
042200     EVALUATE RETURN-CODE                                                 
042300       WHEN 0  PERFORM CNA-REFRESH-PANEL                                  
042400               MOVE SPACE TO  ISPF-ZZ1LCMD ISPF-EXTRACTID-1               
042500               MOVE MSG-VIOS101A  TO ISPF-MSG-ID                          
042600       WHEN 4  MOVE SPACE TO  ISPF-ZZ1LCMD ISPF-EXTRACTID-1               
042700               MOVE MSG-VIOS101B  TO ISPF-MSG-ID                          
042800       WHEN 7  MOVE MSG-VIOS101U  TO ISPF-MSG-ID                          
042810       WHEN 8  MOVE MSG-VIOS101C  TO ISPF-MSG-ID                          
042820       WHEN 9  MOVE MSG-VIOS101V  TO ISPF-MSG-ID                          
042900       WHEN 12 MOVE MSG-VIOS101G  TO ISPF-MSG-ID                          
043000       WHEN OTHER MOVE YES TO SW-ERROR                                    
043100     END-EVALUATE                                                         
043200     CONTINUE.                                                            
043300     EJECT                                                                
043400*--------------------------------------------------------------           
043500 CNA-REFRESH-PANEL SECTION.                                               
043600*--------------------------------------------------------------           
043700     MOVE 'CNA-REFRESH-PANEL        ' TO ABEND-SECTION                    
043800D    DISPLAY ABEND-SECTION                                                
043900     SKIP2                                                                
044000     PERFORM SQL-SELECT-EXTRACT                                           
044100     IF SQLCODE = +000                                                    
044200       MOVE EXTRACT-EXTRACTID   TO ISPF-EXTRACTID                         
044300       MOVE EXTRACT-DESCRIPTION TO ISPF-DESCRIPTION                       
044400       MOVE EXTRACT-OWNER       TO ISPF-OWNER                             
044500       MOVE NOO                 TO ISPF-RESTRICTED                        
044600       PERFORM S01-TBADD-ISPFTAB                                          
044700     ELSE                                                                 
044800       MOVE YES TO SW-ERROR                                               
044900     END-IF                                                               
045000     CONTINUE.                                                            
045100     EJECT                                                                
045200*--------------------------------------------------------------           
045300 CV-VIEW-EXTRACTID SECTION.                                               
045400*--------------------------------------------------------------           
045500     MOVE 'CV-VIEW-EXTRACTID       ' TO ABEND-SECTION                     
045600D    DISPLAY ABEND-SECTION                                                
045700     SKIP2                                                                
045800     CALL V161604P USING W-EXTRACTID                                      
045900     EVALUATE RETURN-CODE                                                 
046000       WHEN 0                                                             
046100               MOVE SPACE TO  ISPF-ZZ1LCMD ISPF-EXTRACTID-1               
046200               MOVE MSG-VIOS101A  TO ISPF-MSG-ID                          
046300       WHEN 4                                                             
046400               MOVE SPACE TO  ISPF-ZZ1LCMD ISPF-EXTRACTID-1               
046500               MOVE MSG-VIOS101A  TO ISPF-MSG-ID                          
046600       WHEN 8  MOVE MSG-VIOS101E  TO ISPF-MSG-ID                          
046700       WHEN 12 MOVE MSG-VIOS101G  TO ISPF-MSG-ID                          
046800       WHEN OTHER MOVE YES TO SW-ERROR                                    
046900     END-EVALUATE                                                         
047000     CONTINUE.                                                            
047100     EJECT                                                                
047200*--------------------------------------------------------------           
047300 CU-UPDATE-EXTRACTID SECTION.                                             
047400*--------------------------------------------------------------           
047500     MOVE 'CU-UPDATE-EXTRACTID     ' TO ABEND-SECTION                     
047600D    DISPLAY ABEND-SECTION                                                
047700     SKIP2                                                                
047800     CALL V161603P USING W-EXTRACTID                                      
047900     EVALUATE RETURN-CODE                                                 
048000       WHEN 0  PERFORM CUA-REFRESH-PANEL                                  
048100               MOVE SPACE TO ISPF-ZZ1LCMD ISPF-EXTRACTID-1                
048200               MOVE MSG-VIOS101A TO ISPF-MSG-ID                           
048300       WHEN 4  MOVE SPACE TO ISPF-ZZ1LCMD ISPF-EXTRACTID-1                
048400               MOVE MSG-VIOS101B  TO ISPF-MSG-ID                          
048500       WHEN 8  MOVE MSG-VIOS101D  TO ISPF-MSG-ID                          
048600       WHEN 12 MOVE MSG-VIOS101G  TO ISPF-MSG-ID                          
048700       WHEN OTHER MOVE YES TO SW-ERROR                                    
048800     END-EVALUATE                                                         
048900     CONTINUE.                                                            
049000     EJECT                                                                
049100*--------------------------------------------------------------           
049200 CUA-REFRESH-PANEL SECTION.                                               
049300*--------------------------------------------------------------           
049400     MOVE 'CUA-REFRESH-PANEL        ' TO ABEND-SECTION                    
049500D    DISPLAY ABEND-SECTION                                                
049600     SKIP2                                                                
049700     PERFORM SQL-SELECT-EXTRACT                                           
049800     IF SQLCODE = +000                                                    
049900       MOVE EXTRACT-EXTRACTID   TO ISPF-EXTRACTID                         
050000       PERFORM S01-TBGET-ISPFTAB                                          
050100       IF RCODE = 0                                                       
050200         MOVE EXTRACT-DESCRIPTION TO ISPF-DESCRIPTION                     
050300         MOVE EXTRACT-OWNER       TO ISPF-OWNER                           
050400         MOVE NOO                 TO ISPF-RESTRICTED                      
050500         PERFORM S01-TBMOD-ISPFTAB                                        
050600       END-IF                                                             
050700     ELSE                                                                 
050800       MOVE YES TO SW-ERROR                                               
050900     END-IF                                                               
051000     CONTINUE.                                                            
051100     EJECT                                                                
051200*--------------------------------------------------------------           
051300 CD-DELETE-EXTRACTID SECTION.                                             
051400*--------------------------------------------------------------           
051500     MOVE 'CD-DELETE-EXTRACTID     ' TO ABEND-SECTION                     
051600D    DISPLAY ABEND-SECTION                                                
051700     SKIP2                                                                
051800     CALL V161605P USING W-EXTRACTID                                      
051900     EVALUATE RETURN-CODE                                                 
052000       WHEN 0  PERFORM CDA-REFRESH-PANEL                                  
052100               MOVE SPACE TO  ISPF-ZZ1LCMD ISPF-EXTRACTID-1               
052200               MOVE MSG-VIOS101A  TO ISPF-MSG-ID                          
052300       WHEN 4  MOVE SPACE TO  ISPF-ZZ1LCMD ISPF-EXTRACTID-1               
052400               MOVE MSG-VIOS101B  TO ISPF-MSG-ID                          
052500       WHEN 8  MOVE MSG-VIOS101D  TO ISPF-MSG-ID                          
052600       WHEN 12 MOVE MSG-VIOS101G  TO ISPF-MSG-ID                          
052700       WHEN OTHER MOVE YES TO SW-ERROR                                    
052800     END-EVALUATE                                                         
052900     CONTINUE.                                                            
053000     EJECT                                                                
053100*--------------------------------------------------------------           
053200 CDA-REFRESH-PANEL SECTION.                                               
053300*--------------------------------------------------------------           
053400     MOVE 'CDA-REFRESH-PANEL        ' TO ABEND-SECTION                    
053500D    DISPLAY ABEND-SECTION                                                
053600     SKIP2                                                                
053700       MOVE W-EXTRACTID   TO ISPF-EXTRACTID                               
053800       PERFORM S01-TBDELETE-ISPFTAB                                       
053900     CONTINUE.                                                            
054000     EJECT                                                                
054100*--------------------------------------------------------------           
054200 CS-POPUP SECTION.                                                        
054300*--------------------------------------------------------------           
054400     MOVE 'CS-POPUP            ' TO ABEND-SECTION                         
054500D    DISPLAY ABEND-SECTION                                                
054600     SKIP2                                                                
054700     MOVE 'ACTIONS FOR EXTRACTS' TO ISPF-ZWINTTL                          
054800     PERFORM S01-ADDPOP                                                   
054900     PERFORM CSA-DISPLAY-POPUP                                            
055000     PERFORM UNTIL SW-ENTER-PRESSED = NOO OR                              
055100                   ISPF-ZZLCMD NOT = SPACE                                
055200                   OR SW-ERROR = YES                                      
055300         PERFORM CSA-DISPLAY-POPUP                                        
055400     END-PERFORM                                                          
055500     PERFORM S01-REMPOP                                                   
055600     MOVE ISPF-ZZLCMD TO W-ZZLCMD                                         
055700     IF SW-ENTER-PRESSED = NOO                                            
055800        MOVE SPACE TO ISPF-ZZ1LCMD, W-ZZLCMD                              
055900        MOVE MSG-VIOS101B  TO ISPF-MSG-ID                                 
056000     END-IF                                                               
056100     CONTINUE.                                                            
056200     EJECT                                                                
056300*--------------------------------------------------------------           
056400 CSA-DISPLAY-POPUP SECTION.                                               
056500*--------------------------------------------------------------           
056600     MOVE 'CSA-DISPLAY-POPUP           ' TO ABEND-SECTION                 
056700D    DISPLAY ABEND-SECTION                                                
056800     SKIP2                                                                
056900     IF SW-EXTRACT-LISTED = NOO                                           
057000        MOVE V1616002 TO ISPF-PANEL                                       
057100     ELSE                                                                 
057200        MOVE V1616003 TO ISPF-PANEL                                       
057300     END-IF                                                               
057400     PERFORM S01-DISPLAY-PANEL                                            
057500     IF RETURN-CODE <= 4                                                  
057600        MOVE YES TO SW-ENTER-PRESSED                                      
057700     ELSE                                                                 
057800        MOVE NOO TO SW-ENTER-PRESSED                                      
057900        IF RETURN-CODE > 8                                                
058000           MOVE YES TO SW-ERROR                                           
058100        END-IF                                                            
058200     END-IF                                                               
058300     CONTINUE.                                                            
058400     EJECT                                                                
058500*--------------------------------------------------------------           
058600 Z-FINIT SECTION.                                                         
058700*--------------------------------------------------------------           
058800     MOVE 'Z-FINIT                  ' TO ABEND-SECTION                    
058900D    DISPLAY ABEND-SECTION                                                
059000     SKIP2                                                                
059100     PERFORM ZA-VDEL-GENERAL                                              
059200     PERFORM ZB-VDEL-PANEL-V161600M                                       
059300     MOVE RCODE TO RETURN-CODE                                            
059400     CONTINUE.                                                            
059500     EJECT                                                                
059600*--------------------------------------------------------------           
059700 ZA-VDEL-GENERAL SECTION.                                                 
059800*--------------------------------------------------------------           
059900     MOVE 'ZA-VDEL-GENERAL       ' TO ABEND-SECTION                       
060000D    DISPLAY ABEND-SECTION                                                
060100     SKIP2                                                                
060200     CALL 'ISPLINK' USING VDELETE N-SMSG                                  
060300     CALL 'ISPLINK' USING VDELETE N-LMSG                                  
060400     CALL 'ISPLINK' USING VDELETE N-ZCMD                                  
060500     CALL 'ISPLINK' USING VDELETE N-ZWINTTL                               
060600     CALL 'ISPLINK' USING VDELETE N-ZZ1LCMD                               
060700     CALL 'ISPLINK' USING VDELETE N-ZZLCMD                                
060800     CONTINUE.                                                            
060900     EJECT                                                                
061000*--------------------------------------------------------------           
061100 ZB-VDEL-PANEL-V161600M SECTION.                                          
061200*--------------------------------------------------------------           
061300     MOVE 'ZB-VDEL-PANEL-V161600M    ' TO ABEND-SECTION                   
061400D    DISPLAY ABEND-SECTION                                                
061500     SKIP2                                                                
061600     CALL 'ISPLINK' USING VDELETE N-EXTRACTID-1                           
061700     CALL 'ISPLINK' USING VDELETE N-EXTRACTID                             
061800     CALL 'ISPLINK' USING VDELETE N-DESCRIPTION                           
061900     CALL 'ISPLINK' USING VDELETE N-RESTRICTED                            
062000     CONTINUE.                                                            
062100     EJECT                                                                
062200*--------------------------------------------------------------           
062300 S01-DISPLAY-PANEL SECTION.                                               
062400*--------------------------------------------------------------           
062500     MOVE 'S01-DISPLAY-PANEL           ' TO ABEND-SECTION                 
062600D    DISPLAY ABEND-SECTION                                                
062700     SKIP2                                                                
062800     CALL 'ISPLINK' USING DISPLAYE ISPF-PANEL ISPF-MSG-ID                 
062900     MOVE RETURN-CODE TO RCODE RCODE-DISPL                                
063000     MOVE SPACE TO ISPF-MSG-ID                                            
063100     IF RCODE > +8                                                        
063200        PERFORM S99-ERROR-ROUTINE                                         
063300        MOVE YES TO SW-ERROR                                              
063400        MOVE RCODE-16 TO RCODE                                            
063500     END-IF                                                               
063600     CONTINUE.                                                            
063700     EJECT                                                                
063800*--------------------------------------------------------------           
063900 S01-TBCREATE-ISPFTAB SECTION.                                            
064000*--------------------------------------------------------------           
064100     MOVE 'S01-TBCREATE                ' TO ABEND-SECTION                 
064200D    DISPLAY ABEND-SECTION                                                
064300     SKIP2                                                                
064400     CALL 'ISPLINK' USING TBCREATE                                        
064500                          EXTRACTTAB-NAME                                 
064600                          EXTRACTTAB-KEY                                  
064700                          EXTRACTTAB-VAR                                  
064800                          TABNOW                                          
064900                          TABREP                                          
065000     MOVE RETURN-CODE TO RCODE RCODE-DISPL                                
065100     IF RCODE <= +8                                                       
065200        MOVE NOO TO SW-ERROR                                              
065300     ELSE                                                                 
065400        MOVE YES TO SW-ERROR                                              
065500        PERFORM S99-ERROR-ROUTINE                                         
065600        MOVE RCODE-16 TO RCODE                                            
065700     END-IF                                                               
065800     CONTINUE.                                                            
065900     EJECT                                                                
066000*--------------------------------------------------------------           
066100 S01-TBDISPL-ISPFTAB SECTION.                                             
066200*--------------------------------------------------------------           
066300     MOVE 'S01-TBDISPL-ISPFTAB         ' TO ABEND-SECTION                 
066400D    DISPLAY ABEND-SECTION                                                
066500     SKIP2                                                                
066600     CALL 'ISPLINK' USING TBDISPL EXTRACTTAB-NAME V1616001                
066700                          ISPF-MSG-ID                                     
066800     MOVE RETURN-CODE TO RCODE RCODE-DISPL                                
066900     MOVE SPACE TO ISPF-MSG-ID                                            
067000     IF RCODE <= +8                                                       
067100        MOVE NOO TO SW-ERROR                                              
067200     ELSE                                                                 
067300        MOVE YES TO SW-ERROR                                              
067400        PERFORM S99-ERROR-ROUTINE                                         
067500        MOVE RCODE-16 TO RCODE                                            
067600     END-IF                                                               
067700     CONTINUE.                                                            
067800     EJECT                                                                
067900*--------------------------------------------------------------           
068000 S01-TBADD-ISPFTAB SECTION.                                               
068100*--------------------------------------------------------------           
068200     MOVE 'S01-TBADD-ISPFTAB           ' TO ABEND-SECTION                 
068300D    DISPLAY ABEND-SECTION                                                
068400     SKIP2                                                                
068500     CALL 'ISPLINK' USING TBADD                                           
068600                          EXTRACTTAB-NAME                                 
068700     MOVE RETURN-CODE TO RCODE RCODE-DISPL                                
068800     IF RCODE = +0                                                        
068900        MOVE NOO TO SW-ERROR                                              
069000     ELSE                                                                 
069100        MOVE YES TO SW-ERROR                                              
069200        PERFORM S99-ERROR-ROUTINE                                         
069300        MOVE RCODE-16 TO RCODE                                            
069400     END-IF                                                               
069500     CONTINUE.                                                            
069600     EJECT                                                                
069700*--------------------------------------------------------------           
069800 S01-TBMOD-ISPFTAB SECTION.                                               
069900*--------------------------------------------------------------           
070000     MOVE 'S01-TBMOD-ISPFTAB           ' TO ABEND-SECTION                 
070100D    DISPLAY ABEND-SECTION                                                
070200     SKIP2                                                                
070300     CALL 'ISPLINK' USING TBMOD                                           
070400                          EXTRACTTAB-NAME                                 
070500     MOVE RETURN-CODE TO RCODE RCODE-DISPL                                
070600     IF RCODE = +0                                                        
070700        MOVE NOO TO SW-ERROR                                              
070800     ELSE                                                                 
070900        MOVE YES TO SW-ERROR                                              
071000        PERFORM S99-ERROR-ROUTINE                                         
071100        MOVE RCODE-16 TO RCODE                                            
071200     END-IF                                                               
071300     CONTINUE.                                                            
071400     EJECT                                                                
071500*--------------------------------------------------------------           
071600 S01-TBGET-ISPFTAB SECTION.                                               
071700*--------------------------------------------------------------           
071800     MOVE 'S01-TBGET-ISPFTAB           ' TO ABEND-SECTION                 
071900D    DISPLAY ABEND-SECTION                                                
072000     SKIP2                                                                
072100     CALL 'ISPLINK' USING TBGET                                           
072200                          EXTRACTTAB-NAME                                 
072300     MOVE RETURN-CODE TO RCODE RCODE-DISPL                                
072400     IF RCODE <= +8                                                       
072500        MOVE NOO TO SW-ERROR                                              
072600     ELSE                                                                 
072700        MOVE YES TO SW-ERROR                                              
072800        PERFORM S99-ERROR-ROUTINE                                         
072900        MOVE RCODE-16 TO RCODE                                            
073000     END-IF                                                               
073100     CONTINUE.                                                            
073200     EJECT                                                                
073300*--------------------------------------------------------------           
073400 S01-TBDELETE-ISPFTAB SECTION.                                            
073500*--------------------------------------------------------------           
073600     MOVE 'S01-TBDELETE-ISPFTAB        ' TO ABEND-SECTION                 
073700D    DISPLAY ABEND-SECTION                                                
073800     SKIP2                                                                
073900     CALL 'ISPLINK' USING TBDELETE                                        
074000                          EXTRACTTAB-NAME                                 
074100     MOVE RETURN-CODE TO RCODE RCODE-DISPL                                
074200     IF RCODE <= +8                                                       
074300        MOVE NOO TO SW-ERROR                                              
074400     ELSE                                                                 
074500        MOVE YES TO SW-ERROR                                              
074600        PERFORM S99-ERROR-ROUTINE                                         
074700        MOVE RCODE-16 TO RCODE                                            
074800     END-IF                                                               
074900     CONTINUE.                                                            
075000     EJECT                                                                
075100*--------------------------------------------------------------           
075200 S01-TBTOP-ISPFTAB SECTION.                                               
075300*--------------------------------------------------------------           
075400     MOVE 'S01-TBTOP-ISPFTAB           ' TO ABEND-SECTION                 
075500D    DISPLAY ABEND-SECTION                                                
075600     SKIP2                                                                
075700     CALL 'ISPLINK' USING TBTOP                                           
075800                          EXTRACTTAB-NAME                                 
075900     MOVE RETURN-CODE TO RCODE RCODE-DISPL                                
076000     IF RCODE = +0                                                        
076100        MOVE NOO TO SW-ERROR                                              
076200     ELSE                                                                 
076300        MOVE YES TO SW-ERROR                                              
076400        PERFORM S99-ERROR-ROUTINE                                         
076500        MOVE RCODE-16 TO RCODE                                            
076600     END-IF                                                               
076700     CONTINUE.                                                            
076800     EJECT                                                                
076900*--------------------------------------------------------------           
077000 S01-ADDPOP SECTION.                                                      
077100*--------------------------------------------------------------           
077200     MOVE 'S01-ADDPOP                  ' TO ABEND-SECTION                 
077300D    DISPLAY ABEND-SECTION                                                
077400     SKIP2                                                                
077500     CALL 'ISPLINK' USING ADDPOP W-EMPTY-PANEL                            
077600                                 ISPF-ROW-POS ISPF-COL-POS                
077700     MOVE RETURN-CODE TO RCODE RCODE-DISPL                                
077800     IF RCODE = +0                                                        
077900        MOVE NOO TO SW-ERROR                                              
078000     ELSE                                                                 
078100        MOVE YES TO SW-ERROR                                              
078200        PERFORM S99-ERROR-ROUTINE                                         
078300        MOVE RCODE-16 TO RCODE                                            
078400     END-IF                                                               
078500     CONTINUE.                                                            
078600     EJECT                                                                
078700*--------------------------------------------------------------           
078800 S01-REMPOP SECTION.                                                      
078900*--------------------------------------------------------------           
079000     MOVE 'S01-REMPOP                  ' TO ABEND-SECTION                 
079100D    DISPLAY ABEND-SECTION                                                
079200     SKIP2                                                                
079300     CALL 'ISPLINK' USING REMPOP                                          
079400     MOVE RETURN-CODE TO RCODE RCODE-DISPL                                
079500     IF RCODE = +0                                                        
079600        MOVE NOO TO SW-ERROR                                              
079700     ELSE                                                                 
079800        MOVE YES TO SW-ERROR                                              
079900        PERFORM S99-ERROR-ROUTINE                                         
080000        MOVE RCODE-16 TO RCODE                                            
080100     END-IF                                                               
080200     CONTINUE.                                                            
080300     EJECT                                                                
080400*--------------------------------------------------------------           
080500 S99-ERROR-ROUTINE SECTION.                                               
080600*--------------------------------------------------------------           
080700     SKIP2                                                                
080800     STRING 'ERROR IN ' ABEND-SECTION ' RCODE ='                          
080900     RCODE-DISPL DELIMITED BY SIZE                                        
081000     INTO W-ERROR-MESSAGE                                                 
081100     DISPLAY W-ERROR-MESSAGE                                              
081200     CONTINUE.                                                            
081300     EJECT                                                                
081400*--------------------------------------------------------------           
081500 SQL-SELECT-EXTRACT SECTION.                                              
081600*--------------------------------------------------------------           
081700     MOVE 'SQL-SELECT-EXTRACT          ' TO ABEND-SECTION                 
081800D    DISPLAY ABEND-SECTION                                                
081900     SKIP2                                                                
082000     EXEC SQL                                                             
082100        SELECT EXTRACTID, DESCRIPTION, OWNER                              
082200        INTO :EXTRACT-EXTRACTID, :EXTRACT-DESCRIPTION,                    
082300             :EXTRACT-OWNER                                               
082400        FROM EXTRACT                                                      
082500        WHERE EXTRACTID = :W-EXTRACTID                                    
082600     END-EXEC                                                             
082700     PERFORM S95-CONTROL-SQLCODE                                          
082800     CONTINUE.                                                            
082900     EJECT                                                                
083000*--------------------------------------------------------------           
083100 SQL-DECLARE-OPEN-EXTRACT SECTION.                                        
083200*--------------------------------------------------------------           
083300     MOVE 'SQL-DECLARE-OPEN-EXTRACT    ' TO ABEND-SECTION                 
083400D    DISPLAY ABEND-SECTION                                                
083500     SKIP2                                                                
083600     IF W-EXTRACTID = SPACE                                               
083700D      DISPLAY 'CRS1'                                                     
083800       EXEC SQL                                                           
083900           DECLARE CRS-EXTRACT-1 CURSOR FOR                               
084000           SELECT EXTRACTID, DESCRIPTION, OWNER                           
084100           FROM EXTRACT                                                   
084200           ORDER BY OWNER, EXTRACTID                                      
084300       END-EXEC                                                           
084400       EXEC SQL                                                           
084500           OPEN CRS-EXTRACT-1                                             
084600       END-EXEC                                                           
084700     ELSE                                                                 
084800       IF SW-EQUAL = YES                                                  
084900D        DISPLAY 'CRS2'                                                   
085000         EXEC SQL                                                         
085100             DECLARE CRS-EXTRACT-2 CURSOR FOR                             
085200             SELECT EXTRACTID, DESCRIPTION, OWNER                         
085300             FROM EXTRACT                                                 
085400             WHERE EXTRACTID LIKE :W-EXTRACTID                            
085500             ORDER BY OWNER, EXTRACTID                                    
085600         END-EXEC                                                         
085700         EXEC SQL                                                         
085800             OPEN CRS-EXTRACT-2                                           
085900         END-EXEC                                                         
086000       ELSE                                                               
086100D        DISPLAY 'CRS3'                                                   
086200         EXEC SQL                                                         
086300             DECLARE CRS-EXTRACT-3 CURSOR FOR                             
086400             SELECT EXTRACTID, DESCRIPTION, OWNER                         
086500             FROM EXTRACT                                                 
086600             WHERE EXTRACTID = :W-EXTRACTID                               
086700         END-EXEC                                                         
086800         EXEC SQL                                                         
086900             OPEN CRS-EXTRACT-3                                           
087000         END-EXEC                                                         
087100       END-IF                                                             
087200     END-IF                                                               
087300     PERFORM S95-CONTROL-SQLCODE                                          
087400     CONTINUE.                                                            
087500     EJECT                                                                
087600*--------------------------------------------------------------           
087700 SQL-FETCH-EXTRACT SECTION.                                               
087800*--------------------------------------------------------------           
087900     MOVE 'SQL-FETCH-EXTRACT           ' TO ABEND-SECTION                 
088000D    DISPLAY ABEND-SECTION                                                
088100     SKIP2                                                                
088200     IF W-EXTRACTID = SPACE                                               
088300D        DISPLAY 'CRS1'                                                   
088400       EXEC SQL                                                           
088500           FETCH CRS-EXTRACT-1 INTO :EXTRACT-EXTRACTID,                   
088600                                  :EXTRACT-DESCRIPTION,                   
088700                                  :EXTRACT-OWNER                          
088800       END-EXEC                                                           
088900     ELSE                                                                 
089000       IF SW-EQUAL = YES                                                  
089100D        DISPLAY 'CRS2'                                                   
089200         EXEC SQL                                                         
089300             FETCH CRS-EXTRACT-2 INTO :EXTRACT-EXTRACTID,                 
089400                                    :EXTRACT-DESCRIPTION,                 
089500                                  :EXTRACT-OWNER                          
089600         END-EXEC                                                         
089700       ELSE                                                               
089800D        DISPLAY 'CRS3'                                                   
089900         EXEC SQL                                                         
090000             FETCH CRS-EXTRACT-3 INTO :EXTRACT-EXTRACTID,                 
090100                                    :EXTRACT-DESCRIPTION,                 
090200                                  :EXTRACT-OWNER                          
090300         END-EXEC                                                         
090400       END-IF                                                             
090500     END-IF                                                               
090600     PERFORM S95-CONTROL-SQLCODE                                          
090700     CONTINUE.                                                            
090800     EJECT                                                                
090900*--------------------------------------------------------------           
091000 SQL-CLOSE-EXTRACT SECTION.                                               
091100*--------------------------------------------------------------           
091200     MOVE 'SQL-CLOSE-EXTRACT           ' TO ABEND-SECTION                 
091300D    DISPLAY ABEND-SECTION                                                
091400     SKIP2                                                                
091500     IF W-EXTRACTID = SPACE                                               
091600       EXEC SQL                                                           
091700           CLOSE CRS-EXTRACT-1                                            
091800       END-EXEC                                                           
091900     ELSE                                                                 
092000       IF SW-EQUAL = YES                                                  
092100         EXEC SQL                                                         
092200             CLOSE CRS-EXTRACT-2                                          
092300         END-EXEC                                                         
092400       ELSE                                                               
092500         EXEC SQL                                                         
092600             CLOSE CRS-EXTRACT-3                                          
092700         END-EXEC                                                         
092800       END-IF                                                             
092900     END-IF                                                               
093000     PERFORM S95-CONTROL-SQLCODE                                          
093100     CONTINUE.                                                            
093200     EJECT                                                                
093300*--------------------------------------------------------------           
093400 SQL-COMMIT SECTION.                                                      
093500*--------------------------------------------------------------           
093600     MOVE 'SQL-COMMIT                  ' TO ABEND-SECTION                 
093700D    DISPLAY ABEND-SECTION                                                
093800     SKIP2                                                                
093900     EXEC SQL                                                             
094000         COMMIT                                                           
094100     END-EXEC                                                             
094200     PERFORM S95-CONTROL-SQLCODE                                          
094300     CONTINUE.                                                            
094400     EJECT                                                                
094500*-------------------------------- DB2 ERROR HANDLING                      
094600*    -COPY V161PS                                                         
094700*++INCLUDE V161PS                                                         
