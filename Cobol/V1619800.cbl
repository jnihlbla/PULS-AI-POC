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
001100*  * CHECKS IF GIVEN EXTRACTID IS A GENERIC ONE                           
001210*                                                                         
001702*                                                                         
001710* * PARMS:                                                                
001720*                                                                         
001730*        EXTRACTID X(8)                                                   
001740*                                                                         
001800*  * RETURNCODES:                                                         
001900*                                                                         
002100*         8 - NO AUTHORIZATION                                            
002200*        12 - INVALID PARM                                                
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
008800     EJECT                                                                
008900*--------------------------------------------------------------           
009000 LINKAGE SECTION.                                                         
009100*--------------------------------------------------------------           
009200 01  PARM-EXTRACTID         PIC X(8).                                     
009300*************************************************************             
009400 PROCEDURE DIVISION USING PARM-EXTRACTID.                                 
009500*************************************************************             
009510D    DISPLAY 'HERE COMES V1619800'                                        
009520                                                                          
009600     PERFORM A-INIT                                                       
009800     PERFORM B-CHECK                                                      
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
013500     SKIP2                                                                
013511     MOVE PARM-EXTRACTID TO W-EXTRACT                                     
013512     SET IX-EXTRACT TO MAX-IX-EXTRACT                                     
013513     MOVE NOO TO SW-LIKE                                                  
013520     PERFORM UNTIL IX-EXTRACT = 0                                         
013521                                                                          
013523       IF W-EXTR (IX-EXTRACT) = SPACE                                     
013524          SET IX-EXTRACT DOWN BY +1                                       
013525       ELSE                                                               
013530          IF W-EXTR (IX-EXTRACT) = '*'                                    
013532             MOVE YES TO SW-LIKE                                          
013533             PERFORM UNTIL IX-EXTRACT >= MAX-IX-EXTRACT                   
013534                MOVE '%' TO W-EXTR  (IX-EXTRACT)                          
013535                SET IX-EXTRACT UP BY +1                                   
013536             END-PERFORM                                                  
013540             SET IX-EXTRACT TO +1                                         
013541          ELSE                                                            
013542             SET IX-EXTRACT TO +1                                         
013550          END-IF                                                          
013556          SET IX-EXTRACT DOWN BY +1                                       
013557       END-IF                                                             
013561                                                                          
013570     END-PERFORM                                                          
013580                                                                          
013610     IF SW-LIKE = YES                                                     
013611D       DISPLAY 'DETTA VAR ETT EXTRACT MED *'                             
013612        MOVE '*' TO ISPF-ASTERISK                                         
013620     ELSE                                                                 
013630        MOVE SPACE TO ISPF-ASTERISK                                       
013701     END-IF                                                               
013721     MOVE RCODE-0 TO RCODE                                                
014400     CONTINUE.                                                            
014500     EJECT                                                                
018820*--------------------------------------------------------------           
018900 Z-FINIT SECTION.                                                         
019000*--------------------------------------------------------------           
019100     MOVE 'Z-FINIT                  ' TO ABEND-SECTION                    
019200D    DISPLAY ABEND-SECTION                                                
019300     SKIP2                                                                
019400     MOVE RCODE TO RETURN-CODE                                            
019500     CONTINUE.                                                            
