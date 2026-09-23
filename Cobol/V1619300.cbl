000100***************************************************************           
000200 ID  DIVISION.                                                            
000300***************************************************************           
000400 PROGRAM-ID.    V1619300.                                                 
000500 AUTHOR.        LEIF LJUNGKVIST                                           
000600 DATE-WRITTEN.  OKTOBER 1993                                              
000700***************************************************************           
000800*                                                                         
000900*REMARKS:                                                                 
001000*                                                                         
001100*  * CHECKS IF CURRENT USER HAS THE 'A'-AUTHORIZATION                     
001110*  * ADMINISTRATE GIVEN EXTRACTID                                         
001210*                                                                         
001300*  * DB2-TABLES:                                                          
001400*                                                                         
001500*        EXTRACT                                                          
001510*        EXTRACT_MASTER                                                   
001600*                                                                         
001610* * SUBPROGRAMS:                                                          
001700*                                                                         
001701*        V16195 - RETURNS LOGONID FOR CURRENT USER (VGETLID)              
001702*                                                                         
001710* * PARMS:                                                                
001720*                                                                         
001730*        EXTRACTID X(8)                                                   
001740*                                                                         
001800*  * RETURNCODES:                                                         
001900*                                                                         
002100*         8 - NO AUTHORIZATION                                            
002200*        12 - INVALID PARM                                                
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
004500 01  PROGRAM-NAME                 PIC X(8)  VALUE 'V1619400'.             
004600 01  ABEND-SECTION                PIC X(25) VALUE SPACE.                  
004700 01  RETURN-CODES.                                                        
004800     03  RCODE                    PIC S9(4) COMP SYNC VALUE ZERO.         
004900     03  RCODE-0                  PIC S9(4) COMP SYNC VALUE 0.            
004910     03  RCODE-4                  PIC S9(4) COMP SYNC VALUE 4.            
005000     03  RCODE-8                  PIC S9(4) COMP SYNC VALUE 8.            
005100     03  RCODE-12                 PIC S9(4) COMP SYNC VALUE 12.           
005200     03  RCODE-16                 PIC S9(4) COMP SYNC VALUE 16.           
005300     03  RCODE-20                 PIC S9(4) COMP SYNC VALUE 20.           
005400     03  RCODE-DISPL              PIC Z(4)-.                              
005500     SKIP2                                                                
005600 01  GENERAL-CONSTANTS.                                                   
005700     03 YES                       PIC X(1)  VALUE 'Y'.                    
005800     03 NOO                       PIC X(1)  VALUE 'N'.                    
005900     SKIP2                                                                
006000 01  SWITCHES.                                                            
006100     03 SW-ERROR                  PIC X(1)  VALUE 'N'.                    
006110     03 SW-LIKE                   PIC X(1)  VALUE 'N'.                    
006200     SKIP2                                                                
006300 01  W-AREAS.                                                             
006400     03 W-ERROR-MESSAGE           PIC X(40) VALUE SPACE.                  
006500     03 W-COMP-TAB1.                                                      
006600        05 W-COMP-1               PIC X(1) OCCURS 8                       
006700                                  INDEXED BY IX-TAB1.                     
006800     03 W-COMP-TAB2.                                                      
006900        05 W-COMP-2               PIC X(1) OCCURS 8                       
006901                                  INDEXED BY IX-TAB2.                     
006902                                                                          
006903     03 MAX-IX-EXTRACT            PIC S9(4) COMP VALUE +8.                
006904     03 W-EXTRACT.                                                        
006905        05 W-EXTR                 PIC X(1) OCCURS 8                       
006906                                  INDEXED BY IX-EXTRACT.                  
006907 01  N-ISPF-VARS.                                                         
006909     03 N-ASTERISK                PIC X(8)  VALUE 'V161940A'.             
006910                                                                          
006911 01  LTH-ISPF-VARS.                                                       
006913     03 LTH-ASTERISK              PIC S9(6) VALUE 1  COMP.                
006914                                                                          
006915 01  ISPF-VARS.                                                           
006917     03 ISPF-ASTERISK             PIC X(1)  VALUE SPACE.                  
006918                                                                          
006919                                                                          
006920     SKIP2                                                                
006921*--------------------------------------------------------------           
006922*ISPF-CONSTANTS                                                           
006923*--------------------------------------------------------------           
006924 01  DISPLAYE                     PIC X(8)  VALUE 'DISPLAY '.             
006925 01  SELECTT                      PIC X(8)  VALUE 'SELECT  '.             
006928 01  CONTROLL                     PIC X(8)  VALUE 'CONTROL '.             
006929 01  SAVEE                        PIC X(8)  VALUE 'SAVE    '.             
006930 01  RESTOREE                     PIC X(8)  VALUE 'RESTORE '.             
006931 01  VDEFINE                      PIC X(8)  VALUE 'VDEFINE '.             
006932 01  VDELETE                      PIC X(8)  VALUE 'VDELETE '.             
006942 01  CHAR                         PIC X(8)  VALUE 'CHAR    '.             
006943                                                                          
006944                                                                          
006945                                                                          
006946 01 DYNAMIC-SUBPROGRAMS.                                                  
006947    03 VGETLID             PIC X(8)  VALUE 'V16195  '.                    
006948 01 VGETLID-PARMS.                                                        
006949    03 VGETLID-LENGTH      PIC S9(4) COMP VALUE ZERO.                     
006950    03 VGETLID-LOGONID     PIC X(8)       VALUE SPACE.                    
007100     SKIP2                                                                
007200*-------------------------------- DB2 ERROR HANDLING                      
007300*    -COPY V161WS                                                         
007400*++INCLUDE V161WS                                                         
007500*-------------------------------- DB2-AREAS                               
007600     EXEC SQL                                                             
007700          INCLUDE SQLCA                                                   
007800     END-EXEC.                                                            
007900     EXEC SQL                                                             
008000          INCLUDE EXTRACT                                                 
008100     END-EXEC.                                                            
008200*    -COPY EXTRACT -PRE EXTRACT-                                          
008300     EJECT                                                                
008301     EXEC SQL                                                             
008310          INCLUDE EXTUSER                                                 
008320     END-EXEC.                                                            
008330*    -COPY EXTUSER -PRE EXTUSER-                                          
008340     EJECT                                                                
008400     EXEC SQL                                                             
008500          INCLUDE EXTMAST                                                 
008600     END-EXEC.                                                            
008700*    -COPY EXTMAST -PRE EXTMAST-                                          
008800     EJECT                                                                
008900*--------------------------------------------------------------           
009000 LINKAGE SECTION.                                                         
009100*--------------------------------------------------------------           
009200 01  PARM-EXTRACTID         PIC X(8).                                     
009300*************************************************************             
009400 PROCEDURE DIVISION USING PARM-EXTRACTID.                                 
009500*************************************************************             
009510D    DISPLAY 'HERE COMES V1619400'                                        
009600     PERFORM A-INIT                                                       
009700     IF SW-ERROR = NOO                                                    
009800       PERFORM B-CHECK1                                                   
009900       IF SW-ERROR = YES                                                  
010000          PERFORM C-CHECK2                                                
010100       END-IF                                                             
010200     END-IF                                                               
010300     PERFORM Z-FINIT                                                      
010400     GOBACK                                                               
010500     CONTINUE.                                                            
010600                                                                          
010700*--------------------------------------------------------------           
010800 A-INIT SECTION.                                                          
010900*--------------------------------------------------------------           
011000     MOVE 'V16194 A-INIT           ' TO ABEND-SECTION                     
011100D    DISPLAY ABEND-SECTION                                                
011200     SKIP2                                                                
011300     MOVE NOO TO SW-ERROR                                                 
011400     PERFORM AA-INIT-PARMS                                                
011410     PERFORM AB-GET-LOGONID                                               
011500     CONTINUE.                                                            
011600     EJECT                                                                
011700*--------------------------------------------------------------           
011800 AA-INIT-PARMS    SECTION.                                                
011900*--------------------------------------------------------------           
012000     MOVE 'AA-INIT-PARMS           ' TO ABEND-SECTION                     
012100D    DISPLAY ABEND-SECTION                                                
012200     SKIP2                                                                
012300D    DISPLAY 'PARM-EXTRACTID    :' PARM-EXTRACTID                         
012400     IF PARM-EXTRACTID = SPACE                                            
012500        MOVE RCODE-12 TO RCODE                                            
012600        MOVE YES TO SW-ERROR                                              
012700     END-IF                                                               
012701     CALL 'ISPLINK' USING VDEFINE N-ASTERISK  ISPF-ASTERISK               
012710                          CHAR LTH-ASTERISK                               
012800     CONTINUE.                                                            
012900     EJECT                                                                
012910*--------------------------------------------------------------           
012920 AB-GET-LOGONID   SECTION.                                                
012930*--------------------------------------------------------------           
012940     MOVE 'AB-GET-LOGONID          ' TO ABEND-SECTION                     
012950D    DISPLAY ABEND-SECTION                                                
012960     SKIP2                                                                
012995     CALL VGETLID USING VGETLID-LENGTH                                    
012996                        VGETLID-LOGONID                                   
012997     MOVE VGETLID-LOGONID TO EXTRACT-OWNER, EXTMAST-USERID                
012999     CONTINUE.                                                            
013000     EJECT                                                                
013010*--------------------------------------------------------------           
013100 B-CHECK1 SECTION.                                                        
013200*--------------------------------------------------------------           
013300     MOVE 'B-CHECK1       ' TO ABEND-SECTION                              
013400D    DISPLAY ABEND-SECTION                                                
013500     SKIP2                                                                
013511     MOVE PARM-EXTRACTID  TO W-EXTRACT                                    
013512     SET IX-EXTRACT TO MAX-IX-EXTRACT                                     
013513     MOVE NOO TO SW-LIKE                                                  
013520     PERFORM UNTIL IX-EXTRACT = 0                                         
013530       IF W-EXTR (IX-EXTRACT) = '*'                                       
013531          MOVE YES TO SW-LIKE                                             
013532          PERFORM UNTIL IX-EXTRACT >= MAX-IX-EXTRACT                      
013533             MOVE '%' TO W-EXTR  (IX-EXTRACT)                             
013534             SET IX-EXTRACT UP BY +1                                      
013535          END-PERFORM                                                     
013540          SET IX-EXTRACT TO +1                                            
013550       END-IF                                                             
013560       SET IX-EXTRACT DOWN BY +1                                          
013570     END-PERFORM                                                          
013580                                                                          
013600     MOVE W-EXTRACT   TO EXTRACT-EXTRACTID                                
013601     MOVE PARM-EXTRACTID TO  EXTRACT-EXTRACTID                            
013610     IF SW-LIKE = YES                                                     
013611D       DISPLAY 'DETTA VAR ETT EXTRACT MED *'                             
013612        MOVE '*' TO ISPF-ASTERISK                                         
013613*       PERFORM SQL-SELECT-LIKE-EXTRACTID                                 
013620     ELSE                                                                 
013630        MOVE SPACE TO ISPF-ASTERISK                                       
013701     END-IF                                                               
013702     PERFORM SQL-SELECT-EXTRACTID                                         
013710     IF SQLCODE = +000                                                    
013720        MOVE NOO TO SW-ERROR                                              
013721        MOVE RCODE-0 TO RCODE                                             
013730     ELSE                                                                 
013740        MOVE YES TO SW-ERROR                                              
013750        MOVE RCODE-8 TO RCODE                                             
014300     END-IF                                                               
014310D    DISPLAY 'V16193' EXTRACT-EXTRACTID SW-ERROR ISPF-ASTERISK            
014400     CONTINUE.                                                            
014500     EJECT                                                                
014600*--------------------------------------------------------------           
014700 C-CHECK2 SECTION.                                                        
014800*--------------------------------------------------------------           
014900     MOVE 'C-CHECK2       ' TO ABEND-SECTION                              
015000D    DISPLAY ABEND-SECTION                                                
015100     SKIP2                                                                
015200     PERFORM SQL-SELECT-PREFIX                                            
015300     IF SQLCODE = +000                                                    
015401        PERFORM CA-COMPARE-STRING                                         
015410        IF SW-ERROR = YES                                                 
015420           MOVE RCODE-8 TO RCODE                                          
015421        ELSE                                                              
015422           MOVE RCODE-0 TO RCODE                                          
015430        END-IF                                                            
015500     ELSE                                                                 
015600        MOVE YES TO SW-ERROR                                              
015700        IF SQLCODE = +100                                                 
015800          MOVE RCODE-8 TO RCODE                                           
015900        END-IF                                                            
016000     END-IF                                                               
016100     CONTINUE.                                                            
016200     EJECT                                                                
018710*--------------------------------------------------------------           
018720 CA-COMPARE-STRING SECTION.                                               
018730*--------------------------------------------------------------           
018740     MOVE 'CA-COMPARE-STRING  ' TO ABEND-SECTION                          
018750D    DISPLAY ABEND-SECTION                                                
018760     SKIP2                                                                
018770     MOVE PARM-EXTRACTID     TO W-COMP-TAB1                               
018780     MOVE EXTMAST-PREFIX     TO W-COMP-TAB2                               
018782     MOVE NOO TO SW-ERROR                                                 
018784     SET IX-TAB1 TO +1                                                    
018785     SET IX-TAB2 TO +1                                                    
018786     PERFORM UNTIL IX-TAB1 > 8 OR SW-ERROR = YES                          
018787       IF W-COMP-1 (IX-TAB1) NOT = W-COMP-2 (IX-TAB2)                     
018788          AND W-COMP-2 (IX-TAB2) NOT = '*'                                
018789          MOVE YES TO SW-ERROR                                            
018790       END-IF                                                             
018793       SET IX-TAB1 UP BY +1                                               
018794       SET IX-TAB2 UP BY +1                                               
018795     END-PERFORM                                                          
018810     CONTINUE.                                                            
018811     EJECT                                                                
018820*--------------------------------------------------------------           
018900 Z-FINIT SECTION.                                                         
019000*--------------------------------------------------------------           
019100     MOVE 'Z-FINIT                  ' TO ABEND-SECTION                    
019200D    DISPLAY ABEND-SECTION                                                
019300     SKIP2                                                                
019400     MOVE RCODE TO RETURN-CODE                                            
019500     CONTINUE.                                                            
019600     EJECT                                                                
019700*--------------------------------------------------------------           
019800 SQL-SELECT-EXTRACTID SECTION.                                            
019900*--------------------------------------------------------------           
020000     MOVE 'SQL-SELECT-EXTRACTID        ' TO ABEND-SECTION                 
020100D    DISPLAY ABEND-SECTION                                                
020200     SKIP2                                                                
020300     EXEC SQL                                                             
020812       SELECT DISTINCT EXTRACTID                                          
020813       INTO :EXTUSER-EXTRACTID                                            
020814       FROM EXTRACT_USER                                                  
020820       WHERE EXTRACTID = :EXTRACT-EXTRACTID                               
020821       AND USERID = :EXTRACT-OWNER                                        
020822       AND AUTH = 'A'                                                     
020900     END-EXEC                                                             
021000     PERFORM S95-CONTROL-SQLCODE                                          
021100     CONTINUE.                                                            
021110     EJECT                                                                
021120*--------------------------------------------------------------           
021130 SQL-SELECT-LIKE-EXTRACTID SECTION.                                       
021140*--------------------------------------------------------------           
021150     MOVE 'SQL-SELECT-LIKE-EXTRACTID   ' TO ABEND-SECTION                 
021160D    DISPLAY ABEND-SECTION                                                
021170     SKIP2                                                                
021180     EXEC SQL                                                             
021190        SELECT  EXTRACTID                                                 
021191        INTO :EXTRACT-EXTRACTID                                           
021192        FROM EXTRACT                                                      
021193        WHERE EXTRACTID LIKE :EXTRACT-EXTRACTID                           
021194        AND   OWNER     = :EXTRACT-OWNER                                  
021195     END-EXEC                                                             
021196     MOVE 'N' TO C-811A                                                   
021197     MOVE 'N' TO C-811B                                                   
021198     PERFORM S95-CONTROL-SQLCODE                                          
021199     MOVE 'Y' TO C-811A                                                   
021200     MOVE 'Y' TO C-811B                                                   
021201     CONTINUE.                                                            
021210     EJECT                                                                
021300*--------------------------------------------------------------           
021400 SQL-SELECT-PREFIX  SECTION.                                              
021500*--------------------------------------------------------------           
021600     MOVE 'SQL-SELECT-PREFIX           ' TO ABEND-SECTION                 
021700D    DISPLAY ABEND-SECTION                                                
021800     SKIP2                                                                
021900     EXEC SQL                                                             
022000        SELECT  PREFIX                                                    
022100        INTO :EXTMAST-PREFIX                                              
022200        FROM EXTRACT_MASTER                                               
022300        WHERE USERID = :EXTMAST-USERID                                    
022400     END-EXEC                                                             
022500     PERFORM S95-CONTROL-SQLCODE                                          
022600     CONTINUE.                                                            
022700     EJECT                                                                
022800*-------------------------------- DB2 FELHANTERING SEKTION                
022900*    -COPY V161PS                                                         
023000*++INCLUDE V161PS                                                         
