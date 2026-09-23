000100***************************************************************           
000200 ID  DIVISION.                                                            
000300***************************************************************           
000400 PROGRAM-ID.    V1619700.                                                 
000500 AUTHOR.        CARINA HOLMQVIST                                          
000600                CALLS AETTSF INSTEAD, ANDERS THEODORSSON MAR -93.         
000700 DATE-WRITTEN.  DECEMBER  1991                                            
000800***************************************************************           
000900*                                                                         
001000*REMARKS:                                                                 
001100*                                                                         
001200*  * RETURNS NAME FOR GIVEN USERID                                        
001300*                                                                         
001400* * PARMS:                                                                
001500*                                                                         
001600*       USERID X(7)                                                       
001700*                                                                         
001800*  * RETURNCODES:                                                         
001900*                                                                         
002000*          4 - NAME FOR USERID NOT FOUND                                  
002100*         12 - INVALID PARMS                                              
002200*                                                                         
002300***************************************************************           
002400     EJECT                                                                
002500***************************************************************           
002600 ENVIRONMENT DIVISION.                                                    
002700***************************************************************           
002800     SKIP2                                                                
002900*--------------------------------------------------------------           
003000 CONFIGURATION SECTION.                                                   
003100*--------------------------------------------------------------           
003200 SOURCE-COMPUTER. IBM-370.                                                
003300*SOURCE-COMPUTER. IBM-370 WITH DEBUGGING MODE.                            
003400     SKIP2                                                                
003500***************************************************************           
003600 DATA DIVISION.                                                           
003700***************************************************************           
003800     SKIP2                                                                
003900*--------------------------------------------------------------           
004000 WORKING-STORAGE SECTION.                                                 
004100*--------------------------------------------------------------           
004200 01  PROGRAM-NAME                 PIC X(8)  VALUE 'V1619700'.             
004300 01  AETTSF                       PIC X(8)  VALUE 'AETTSF  '.             
004400 01  ABEND-SECTION                PIC X(25) VALUE SPACE.                  
004500 01  RETURN-CODES.                                                        
004600     03  RCODE                    PIC S9(4) COMP SYNC VALUE ZERO.         
004700     03  RCODE-4                  PIC S9(4) COMP SYNC VALUE 4.            
004800     03  RCODE-8                  PIC S9(4) COMP SYNC VALUE 8.            
004900     03  RCODE-12                 PIC S9(4) COMP SYNC VALUE 12.           
005000     03  RCODE-16                 PIC S9(4) COMP SYNC VALUE 16.           
005100     03  RCODE-20                 PIC S9(4) COMP SYNC VALUE 20.           
005200     03  RCODE-DISPL              PIC Z(4)-.                              
005300     SKIP2                                                                
005400 01  GENERAL-CONSTANTS.                                                   
005500     03 YES                       PIC X(1)  VALUE 'Y'.                    
005600     03 NOO                       PIC X(1)  VALUE 'N'.                    
005700     SKIP2                                                                
005800 01  AET-PARMS.                                                           
005900     03 AET-PARM1                 PIC X(2)  VALUE SPACE.                  
006000     03 AET-PARM2                 PIC X(8)  VALUE SPACE.                  
006100     03 AET-PARM3.                                                        
006200        05 FILLER                 PIC X(34) VALUE SPACE.                  
006300        05 AET-NAMN               PIC X(20) VALUE SPACE.                  
006400        05 FILLER                 PIC X(8)  VALUE SPACE.                  
006500     SKIP2                                                                
006600 01  W-VARIABLES.                                                         
006700     03 W-USERID                  PIC X(7)  VALUE SPACE.                  
006800 01 SWITCHES.                                                             
006900     03 SW-ERROR                  PIC X(1)  VALUE 'N'.                    
007000     EJECT                                                                
007100*--------------------------------------------------------------           
007200 LINKAGE SECTION.                                                         
007300*--------------------------------------------------------------           
007400 01 PARM.                                                                 
007500     03 PARM-USERID            PIC X(7).                                  
007600     03 PARM-NAME              PIC X(20).                                 
007700*************************************************************             
007800 PROCEDURE DIVISION USING PARM.                                           
007900*************************************************************             
008000     PERFORM A-INIT                                                       
008100     IF SW-ERROR = NOO THEN                                               
008200       PERFORM B-GET-NAME                                                 
008300     END-IF                                                               
008400     PERFORM Z-FINIT                                                      
008500     GOBACK                                                               
008600     CONTINUE.                                                            
008700                                                                          
008800*--------------------------------------------------------------           
008900 A-INIT SECTION.                                                          
009000*--------------------------------------------------------------           
009100     MOVE 'A-INIT                  ' TO ABEND-SECTION                     
009200D    DISPLAY ABEND-SECTION                                                
009300     SKIP2                                                                
009400     MOVE NOO TO SW-ERROR                                                 
009500     MOVE ZERO TO RCODE                                                   
009600     PERFORM AA-INIT-PARMS                                                
009700     CONTINUE.                                                            
009800     EJECT                                                                
009900*--------------------------------------------------------------           
010000 AA-INIT-PARMS    SECTION.                                                
010100*--------------------------------------------------------------           
010200     MOVE 'AA-INIT-PARMS           ' TO ABEND-SECTION                     
010300D    DISPLAY ABEND-SECTION                                                
010400     SKIP2                                                                
010500D    DISPLAY 'PARM-USERID ' PARM-USERID                                   
010600     IF PARM-USERID = SPACE                                               
010700        MOVE RCODE-12 TO RCODE                                            
010800        MOVE YES TO SW-ERROR                                              
010900     END-IF                                                               
011000     CONTINUE.                                                            
011100     EJECT                                                                
011200*--------------------------------------------------------------           
011300 B-GET-NAME SECTION.                                                      
011400*--------------------------------------------------------------           
011500     MOVE 'B-GET-NAME              ' TO ABEND-SECTION                     
011600D    DISPLAY ABEND-SECTION                                                
011700     SKIP2                                                                
011800     MOVE '01'        TO AET-PARM1                                        
011900     MOVE PARM-USERID TO AET-PARM2                                        
012000                                                                          
012100     CALL AETTSF USING AET-PARM1                                          
012200                       AET-PARM2                                          
012300                       AET-PARM3                                          
012400     END-CALL                                                             
012500                                                                          
012600     IF RETURN-CODE = 0 THEN                                              
012700       MOVE AET-NAMN TO PARM-NAME                                         
012800     ELSE                                                                 
012900       MOVE RCODE-4 TO RCODE                                              
013000       MOVE SPACE   TO PARM-NAME                                          
013100     END-IF                                                               
013200                                                                          
013300     CONTINUE.                                                            
013400     EJECT                                                                
013500 Z-FINIT SECTION.                                                         
013600*--------------------------------------------------------------           
013700     MOVE 'Z-FINIT                 ' TO ABEND-SECTION                     
013800D    DISPLAY ABEND-SECTION                                                
013900     SKIP2                                                                
014000     MOVE RCODE TO RETURN-CODE                                            
014100     CONTINUE.                                                            
014200     EJECT                                                                
