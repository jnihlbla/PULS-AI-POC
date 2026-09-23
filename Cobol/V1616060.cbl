000100***************************************************************           
000200 ID  DIVISION.                                                            
000300***************************************************************           
000400 PROGRAM-ID.    V1619800.                                                 
000500 AUTHOR.        CARINA HOLMQVIST                                          
000600 DATE-WRITTEN.  SEPTEMBER 1991                                            
000700***************************************************************           
000800*                                                                         
000900*REMARKS:                                                                 
001000*                                                                         
001100*  * CHECKS IF GIVEN EXTRACTID IS A FORMALLY CORRECT                      
001210*                                                                         
001710* * PARMS:                                                                
001720*                                                                         
001730*        EXTRACTID X(8)                                                   
001740*                                                                         
001800*  * RETURNCODES:                                                         
001900*                                                                         
002100*         8 - INVALID EXTRACTID                                           
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
005000     03  RCODE-7                  PIC S9(4) COMP SYNC VALUE 7.            
005010     03  RCODE-8                  PIC S9(4) COMP SYNC VALUE 8.            
005020     03  RCODE-9                  PIC S9(4) COMP SYNC VALUE 9.            
005100     03  RCODE-12                 PIC S9(4) COMP SYNC VALUE 12.           
005200     03  RCODE-16                 PIC S9(4) COMP SYNC VALUE 16.           
005300     03  RCODE-20                 PIC S9(4) COMP SYNC VALUE 20.           
005400     03  RCODE-DISPL              PIC Z(4)9-.                             
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
006905     03 W-EXTRACT.                                                        
006906        05 W-EXTR                 PIC X(1) OCCURS 8                       
006907                                  INDEXED BY IX-EXTRACT.                  
006908                                                                          
006909     03 W-EXTRACT-1               PIC X(8).                               
006910     03 W-EXTRACT-2               PIC X(8).                               
006911                                                                          
006912 01  N-ISPF-VARS.                                                         
006913     03 N-ASTERISK                PIC X(8)  VALUE 'V161940A'.             
006914                                                                          
006915 01  LTH-ISPF-VARS.                                                       
006916     03 LTH-ASTERISK              PIC S9(6) VALUE 1  COMP.                
006917                                                                          
006918 01  ISPF-VARS.                                                           
006919     03 ISPF-ASTERISK             PIC X(1)  VALUE SPACE.                  
006920                                                                          
006921                                                                          
006922     SKIP2                                                                
006923*--------------------------------------------------------------           
006924*ISPF-CONSTANTS                                                           
006925*--------------------------------------------------------------           
006926 01  DISPLAYE                     PIC X(8)  VALUE 'DISPLAY '.             
006927 01  SELECTT                      PIC X(8)  VALUE 'SELECT  '.             
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
008800     EJECT                                                                
008801                                                                          
008802*-------------------------------- DB2 ERROR HANDLING                      
008803*    -COPY V161WS                                                         
008804*++INCLUDE V161WS                                                         
008805*-------------------------------- DB2-AREAS                               
008806     EXEC SQL                                                             
008807          INCLUDE SQLCA                                                   
008808     END-EXEC.                                                            
008809     EXEC SQL                                                             
008810          INCLUDE EXTRACT                                                 
008811     END-EXEC.                                                            
008812*    -COPY EXTRACT -PRE EXTRACT-                                          
008820     EJECT                                                                
008830                                                                          
008900*--------------------------------------------------------------           
009000 LINKAGE SECTION.                                                         
009100*--------------------------------------------------------------           
009200 01  PARM-EXTRACTID         PIC X(8).                                     
009300*************************************************************             
009400 PROCEDURE DIVISION USING PARM-EXTRACTID.                                 
009500*************************************************************             
009501                                                                          
009510D    DISPLAY 'HERE COMES V1616060 WITH ' PARM-EXTRACTID                   
009520                                                                          
009600     PERFORM A-INIT                                                       
009800     PERFORM B-CHECK                                                      
009900                                                                          
010300     PERFORM Z-FINIT                                                      
010310                                                                          
010400     GOBACK                                                               
010500     CONTINUE.                                                            
010600                                                                          
010700*--------------------------------------------------------------           
010800 A-INIT SECTION.                                                          
010900*--------------------------------------------------------------           
011000     MOVE 'V16198 A-INIT           ' TO ABEND-SECTION                     
011100D    DISPLAY ABEND-SECTION                                                
011200     SKIP2                                                                
011900                                                                          
012400     IF PARM-EXTRACTID = SPACE                                            
012500        MOVE RCODE-12 TO RCODE                                            
012600        MOVE YES TO SW-ERROR                                              
012700     END-IF                                                               
012701     CALL 'ISPLINK' USING VDEFINE N-ASTERISK  ISPF-ASTERISK               
012710                          CHAR LTH-ASTERISK                               
012800     CONTINUE.                                                            
013000     EJECT                                                                
013010*--------------------------------------------------------------           
013100 B-CHECK  SECTION.                                                        
013200*--------------------------------------------------------------           
013300     MOVE 'B-CHECK        ' TO ABEND-SECTION                              
013400D    DISPLAY ABEND-SECTION                                                
013410                                                                          
013412     PERFORM BA-CHECK-FORM                                                
013413                                                                          
013414     IF SW-ERROR = NOO                                                    
013416        PERFORM BB-CHECK-EXTRACTS                                         
013422     END-IF                                                               
013423                                                                          
013424     CONTINUE.                                                            
013430     EJECT                                                                
013440*--------------------------------------------------------------           
013450 BA-CHECK-FORM SECTION.                                                   
013460*--------------------------------------------------------------           
013470     MOVE 'BA-CHECK-FORM   ' TO ABEND-SECTION                             
013480D    DISPLAY ABEND-SECTION                                                
013500                                                                          
013510     MOVE NOO TO SW-ERROR                                                 
013511     MOVE PARM-EXTRACTID TO W-EXTRACT                                     
013512     MOVE PARM-EXTRACTID TO W-EXTRACT-1                                   
013513                                                                          
013514     IF W-EXTR (1) NOT  ALPHABETIC                                        
013515        MOVE YES TO SW-ERROR                                              
013516     ELSE                                                                 
013517        SET IX-EXTRACT TO +2                                              
013520        PERFORM UNTIL IX-EXTRACT > MAX-IX-EXTRACT                         
013521                                                                          
013523          IF W-EXTR (IX-EXTRACT) = SPACE OR                               
013524             W-EXTR (IX-EXTRACT) = '*'                                    
013525             SET IX-EXTRACT UP BY +1                                      
013526             PERFORM UNTIL IX-EXTRACT > MAX-IX-EXTRACT                    
013527                IF W-EXTR (IX-EXTRACT) NOT = SPACE                        
013528                   MOVE YES TO SW-ERROR                                   
013529                   SET IX-EXTRACT TO MAX-IX-EXTRACT                       
013532                END-IF                                                    
013533                SET IX-EXTRACT UP BY +1                                   
013534             END-PERFORM                                                  
013535          ELSE                                                            
013536             IF NOT (W-EXTR (IX-EXTRACT)  ALPHABETIC OR                   
013537                     W-EXTR (IX-EXTRACT)  NUMERIC )                       
013538                MOVE YES TO SW-ERROR                                      
013539                SET IX-EXTRACT TO MAX-IX-EXTRACT                          
013550             END-IF                                                       
013551             SET IX-EXTRACT UP BY +1                                      
013557          END-IF                                                          
013561                                                                          
013570        END-PERFORM                                                       
013580                                                                          
013590     END-IF                                                               
013600                                                                          
013610     IF SW-ERROR = YES                                                    
013612        MOVE RCODE-7 TO RCODE                                             
013701     ELSE                                                                 
013721        MOVE RCODE-0 TO RCODE                                             
013722     END-IF                                                               
013723                                                                          
013724     CONTINUE.                                                            
013725     EJECT                                                                
013726*--------------------------------------------------------------           
013727 BB-CHECK-EXTRACTS SECTION.                                               
013728*--------------------------------------------------------------           
013729     MOVE 'BB-CHECK-EXTRACTS ' TO ABEND-SECTION                           
013730D    DISPLAY ABEND-SECTION                                                
013740                                                                          
013741     MOVE NOO TO SW-LIKE                                                  
013742     SET IX-EXTRACT TO +2                                                 
013743     PERFORM UNTIL IX-EXTRACT > MAX-IX-EXTRACT                            
013744                                                                          
013745        IF SW-LIKE = YES                                                  
013746           MOVE '%' TO W-EXTR (IX-EXTRACT)                                
013747        ELSE                                                              
013748           IF W-EXTR (IX-EXTRACT) = '*'                                   
013749              MOVE '%' TO W-EXTR (IX-EXTRACT)                             
013750              MOVE YES TO SW-LIKE                                         
013770           END-IF                                                         
013771        END-IF                                                            
013772        SET IX-EXTRACT UP BY +1                                           
013773                                                                          
013774     END-PERFORM                                                          
013780                                                                          
013790     IF SW-LIKE = YES                                                     
013791        MOVE W-EXTRACT TO W-EXTRACT-2                                     
013800        PERFORM SQL-SELECT-LIKE-EXTRACTID                                 
013810        MOVE SQLCODE TO RCODE-DISPL                                       
013820D       DISPLAY 'V1616060 SQLCODE ' RCODE-DISPL                           
013900        IF SQLCODE NOT = +100                                             
014000           MOVE RCODE-9 TO RCODE                                          
014100        END-IF                                                            
014200     END-IF                                                               
014300                                                                          
014400     CONTINUE.                                                            
014401                                                                          
014402                                                                          
014403     EJECT                                                                
014404*--------------------------------------------------------------           
014405 SQL-SELECT-LIKE-EXTRACTID SECTION.                                       
014406*--------------------------------------------------------------           
014407     MOVE 'SQL-SELECT-LIKE-EXTRACTID   ' TO ABEND-SECTION                 
014408D    DISPLAY ABEND-SECTION                                                
014409D    DISPLAY W-EXTRACT ' ' W-EXTRACT-1 ' ' W-EXTRACT-2                    
014410                                                                          
014411     SKIP2                                                                
014412     EXEC SQL                                                             
014413        SELECT  EXTRACTID                                                 
014414        INTO :W-EXTRACT-2                                                 
014415        FROM EXTRACT                                                      
014416        WHERE EXTRACTID LIKE :W-EXTRACT-2                                 
014417        AND   OWNER  ^= USER                                              
014418     END-EXEC                                                             
014419     MOVE 'N' TO C-811A                                                   
014420     MOVE 'N' TO C-811B                                                   
014421     PERFORM S95-CONTROL-SQLCODE                                          
014422     MOVE 'Y' TO C-811A                                                   
014423     MOVE 'Y' TO C-811B                                                   
014430     CONTINUE.                                                            
014500     EJECT                                                                
018820*--------------------------------------------------------------           
018900 Z-FINIT SECTION.                                                         
019000*--------------------------------------------------------------           
019100     MOVE 'Z-FINIT                  ' TO ABEND-SECTION                    
019200D    DISPLAY ABEND-SECTION                                                
019300     SKIP2                                                                
019400     MOVE RCODE TO RETURN-CODE                                            
019500     CONTINUE.                                                            
019501     EJECT                                                                
019502*-------------------------------- DB2 FELHANTERING SEKTION                
019510*    -COPY V161PS                                                         
019600*++INCLUDE V161PS                                                         
