000010****************************************************************          
000020 ID  DIVISION.                                                            
000030****************************************************************          
000040 PROGRAM-ID.    V1612000.                                                 
000050 AUTHOR.        CARINA HOLMQVIST                                          
000060 DATE-WRITTEN.  SEPTEMBER 1991                                            
000061***************************************************************           
000062*                                                                         
000063*REMARKS:                                                                 
000064*                                                                         
000065*  * RETURNS ON OUTFILE EXTRACT-FILE FOR EXTRACTID SPECIFIED ON           
000067*    INFILE                                                               
000068*                                                                         
000069*  * DB2-TABLES:                                                          
000070*                                                                         
000071*        EXTRACT,EXTRACT_USER                                             
000072*                                                                         
000073*  * SUBPROGRAMS:                                                         
000074*                                                                         
000075*        V16196 - RETURNS EXPANDED NAME OF EXTRACT-FILE                   
000077*                                                                         
000078*  * INFILE:                                                              
000079*                                                                         
000080*        CONTAINS A LIST OF EXTRACTIDS                                    
000082*                                                                         
000083*  * OUTFILE:                                                             
000084*                                                                         
000085*        EXTRACTID,EXTRACT_FILE                                           
000086*                                                                         
000087*        FOR EXTRACTIDS ON INFILE                                         
000088*                                                                         
000089*  * RETURNCODES:                                                         
000090*                                                                         
000091*         4 - NO AUTHORIZATION FOR EXTRACTID                              
000092*        16 - SEVERE FILE-ERROR                                           
000093*        20 - SEVERE DB2-ERROR                                            
000094*                                                                         
000095***************************************************************           
000096     EJECT                                                                
000097****************************************************************          
000098 ENVIRONMENT DIVISION.                                                    
000100****************************************************************          
000110 CONFIGURATION SECTION.                                                   
000120 SOURCE-COMPUTER. IBM-370.                                                
000130*SOURCE-COMPUTER. IBM-370 WITH DEBUGGING MODE.                            
000140                                                                          
000150 INPUT-OUTPUT SECTION.                                                    
000160 FILE-CONTROL.                                                            
000170     SELECT INFILE  ASSIGN TO V16120D1.                                   
000180     SELECT OUTFILE ASSIGN TO V16120D2.                                   
000190     EJECT                                                                
000200****************************************************************          
000210 DATA DIVISION.                                                           
000220****************************************************************          
000230 FILE SECTION.                                                            
000240 FD  INFILE                                                               
000250     BLOCK CONTAINS 0                                                     
000260     RECORDING F                                                          
000270     LABEL RECORD STANDARD.                                               
000280 01  INFILE-POST                  PIC X(80).                              
000290                                                                          
000300 FD  OUTFILE                                                              
000310     BLOCK CONTAINS 0                                                     
000320     RECORDING F                                                          
000330     LABEL RECORD STANDARD.                                               
000340 01  OUTFILE-POST                 PIC X(80).                              
000341     EJECT                                                                
000350                                                                          
000351*-----------------------------------------------------------------        
000360 WORKING-STORAGE SECTION.                                                 
000361*-----------------------------------------------------------------        
000370 01  PROGRAM-NAME                 PIC X(8)  VALUE 'V1612000'.             
000380 01  ABEND-SECTION                PIC X(25) VALUE SPACE.                  
000390 01  SWITCHES.                                                            
000400     03  SW-EOF-INFILE            PIC X(1)  VALUE 'N'.                    
000410     03  SW-INFILE-OPEN           PIC X(1)  VALUE 'N'.                    
000420     03  SW-OUTFILE-OPEN          PIC X(1)  VALUE 'N'.                    
000430     03  SW-EXTRACT-FOUND         PIC X(1)  VALUE 'Y'.                    
000450 01  RETURN-CODES.                                                        
000460     03  RCODE                    PIC S9(4) COMP SYNC VALUE ZERO.         
000470     03  RCODE-4                  PIC S9(4) COMP SYNC VALUE 4.            
000480     03  RCODE-8                  PIC S9(4) COMP SYNC VALUE 8.            
000490     03  RCODE-16                 PIC S9(4) COMP SYNC VALUE 16.           
000500     03  RCODE-20                 PIC S9(4) COMP SYNC VALUE 20.           
000510*                                                                         
000520 01  GENERAL-CONSTANTS.                                                   
000530     03 YES                       PIC X(1)  VALUE 'Y'.                    
000540     03 NOO                       PIC X(1)  VALUE 'N'.                    
000550*                                                                         
000600 01  ERROR-MESSAGES.                                                      
000610     03 EXT01.                                                            
000620        05 FILLER                 PIC X(3)  VALUE '***'.                  
000630        05 MESSAGE-ID             PIC X(5)  VALUE 'EXT01'.                
000640        05 MESSAGE-TEXT           PIC X(25) VALUE                         
000650        'NOT AUTHORIZED           '.                                      
000740*                                                                         
000790     EJECT                                                                
000791 01  DYNAMIC-SUBPROGRAMS.                                                 
000792     03 V16196                    PIC X(8)  VALUE 'V16196  '.             
000793     SKIP2                                                                
000794 01  V16196-PARMS.                                                        
000795     03 V16196-DSNAME             PIC X(44) VALUE SPACE.                  
000796     03 V16196-VALID-NO-DAYS      PIC 999   VALUE ZERO.                   
000797     03 V16196-LAST-UPDATED       PIC X(6)  VALUE SPACE.                  
000798     03 V16196-VALID-TO-DATE      PIC X(6)  VALUE SPACE.                  
000799     EJECT                                                                
000800*-------------------------------- COPY TEXT INFILE                        
000810 01  INFILE-V161002.                                                      
000820     10 INFILE-EXTRACTID          PIC X(8).                               
000830     10 INFILE-FILLER             PIC X(72).                              
000840     SKIP2                                                                
000850*-------------------------------- COPY TEXT OUTFILE                       
000860 01  OUTFILE-V161003.                                                     
000870     10 OUTFILE-EXTRACTID         PIC X(8).                               
000880     10 OUTFILE-EXTRACT-FILE      PIC X(42).                              
000890     10 OUTFILE-FILLER            PIC X(30).                              
000900     SKIP2                                                                
000910*-------------------------------- DB2-AREAS                               
000920     EXEC SQL                                                             
000930          INCLUDE SQLCA                                                   
000940     END-EXEC.                                                            
000950     EXEC SQL                                                             
000960          INCLUDE EXTRACT                                                 
000970     END-EXEC.                                                            
000980*    -COPY EXTRACT -PRE EXTRACT-                                          
000990*++INCLUDE EXTRACT                                                        
001000     EXEC SQL                                                             
001010          INCLUDE EXTUSER                                                 
001020     END-EXEC.                                                            
001030*    -COPY EXTUSER -PRE USER-                                             
001040*++INCLUDE EXTUSER                                                        
001050*-------------------------------- DB2 ERROR HANDLING                      
001060*    -COPY V161WS                                                         
001070*++INCLUDE V161WS                                                         
001071     EJECT                                                                
001080                                                                          
001090*************************************************************             
001100 PROCEDURE DIVISION.                                                      
001110*************************************************************             
001120     PERFORM A-INIT                                                       
001130     PERFORM S10-READ-INFILE                                              
001140     PERFORM UNTIL SW-EOF-INFILE = YES                                    
001160        MOVE INFILE-EXTRACTID TO EXTRACT-EXTRACTID                        
001170        PERFORM T15-SELECT-EXTFILE                                        
001171        IF SW-EXTRACT-FOUND = NOO THEN                                    
001181          MOVE RCODE-4          TO RCODE                                  
001250        END-IF                                                            
001255        PERFORM B-INIT-OUTFILE-AREA                                       
001260        PERFORM S10-WRITE-OUTFILE                                         
001300        PERFORM S10-READ-INFILE                                           
001310     END-PERFORM                                                          
001320     PERFORM Z-FINIT                                                      
001330     GOBACK                                                               
001340     CONTINUE.                                                            
001350     EJECT                                                                
001361                                                                          
001362*-----------------------------------------------------------------        
001363 A-INIT SECTION.                                                          
001364*-----------------------------------------------------------------        
001370     MOVE 'A-INIT                   ' TO ABEND-SECTION                    
001380D    DISPLAY ABEND-SECTION                                                
001390     SKIP2                                                                
001430     PERFORM S05-OPEN-INFILE                                              
001440     PERFORM S05-OPEN-OUTFILE                                             
001450     CONTINUE.                                                            
001460     EJECT                                                                
001461                                                                          
001462*-----------------------------------------------------------------        
001463 B-INIT-OUTFILE-AREA SECTION.                                             
001464*-----------------------------------------------------------------        
001465     MOVE 'B-INIT-OUTFILE-AREA      ' TO ABEND-SECTION                    
001466D    DISPLAY ABEND-SECTION                                                
001468     MOVE SPACE            TO OUTFILE-V161003                             
001469     MOVE INFILE-EXTRACTID TO OUTFILE-EXTRACTID                           
001471     IF SW-EXTRACT-FOUND = YES THEN                                       
001472       MOVE EXTRACT-EXTRACT-FILE   TO V16196-DSNAME                       
001473       MOVE ZERO                   TO V16196-VALID-NO-DAYS                
001474       MOVE SPACE                  TO V16196-LAST-UPDATED                 
001475       MOVE SPACE                  TO V16196-VALID-TO-DATE                
001476       CALL V16196 USING V16196-DSNAME                                    
001477                         V16196-VALID-NO-DAYS                             
001478                         V16196-LAST-UPDATED                              
001479                         V16196-VALID-TO-DATE                             
001480       IF RETURN-CODE = 0 THEN                                            
001484          MOVE V16196-DSNAME       TO OUTFILE-EXTRACT-FILE                
001485       END-IF                                                             
001486     ELSE                                                                 
001487        MOVE EXT01                 TO OUTFILE-EXTRACT-FILE                
001488     END-IF                                                               
001494     SKIP2                                                                
001495     CONTINUE.                                                            
001496     EJECT                                                                
001497                                                                          
001498*-----------------------------------------------------------------        
001499 Z-FINIT SECTION.                                                         
001500*-----------------------------------------------------------------        
001501     MOVE 'Z-FINIT                  ' TO ABEND-SECTION                    
001502D    DISPLAY ABEND-SECTION                                                
001503     SKIP2                                                                
001510     IF SW-INFILE-OPEN = YES THEN                                         
001520        PERFORM S15-CLOSE-INFILE                                          
001530     END-IF                                                               
001540     IF SW-OUTFILE-OPEN = YES THEN                                        
001550        PERFORM S15-CLOSE-OUTFILE                                         
001560     END-IF                                                               
001570     MOVE RCODE TO RETURN-CODE                                            
001580     CONTINUE.                                                            
001590     EJECT                                                                
001591                                                                          
001592*-----------------------------------------------------------------        
001600 S05-OPEN-INFILE SECTION.                                                 
001601*-----------------------------------------------------------------        
001610     MOVE 'S05-OPEN-INFILE           ' TO ABEND-SECTION                   
001620D    DISPLAY ABEND-SECTION                                                
001630     SKIP2                                                                
001640     MOVE NOO TO SW-INFILE-OPEN                                           
001650     OPEN INPUT INFILE                                                    
001660     IF RETURN-CODE = 0 THEN                                              
001670        MOVE YES TO SW-INFILE-OPEN                                        
001680     ELSE                                                                 
001690        MOVE RCODE-16 TO RCODE                                            
001700        PERFORM Z-FINIT                                                   
001710        GOBACK                                                            
001720     END-IF                                                               
001730     CONTINUE.                                                            
001740     EJECT                                                                
001741                                                                          
001742*-----------------------------------------------------------------        
001750 S05-OPEN-OUTFILE SECTION.                                                
001751*-----------------------------------------------------------------        
001760     MOVE 'S05-OPEN-OUTFILE           ' TO ABEND-SECTION                  
001770D    DISPLAY ABEND-SECTION                                                
001780     SKIP2                                                                
001790     MOVE NOO TO SW-OUTFILE-OPEN                                          
001800     OPEN OUTPUT OUTFILE                                                  
001810     IF RETURN-CODE = 0 THEN                                              
001820        MOVE YES TO SW-OUTFILE-OPEN                                       
001830     ELSE                                                                 
001840        MOVE RCODE-16 TO RCODE                                            
001850        PERFORM Z-FINIT                                                   
001860        GOBACK                                                            
001870     END-IF                                                               
001880     CONTINUE.                                                            
001890     EJECT                                                                
001891                                                                          
001892*-----------------------------------------------------------------        
001900 S10-READ-INFILE SECTION.                                                 
001901*-----------------------------------------------------------------        
001910     MOVE 'S10-READ-INFILE           ' TO ABEND-SECTION                   
001920D    DISPLAY ABEND-SECTION                                                
001930     SKIP2                                                                
001940     READ                                                                 
001950        INFILE INTO INFILE-V161002                                        
001960        AT END MOVE YES TO SW-EOF-INFILE                                  
001970     END-READ                                                             
001980     IF RETURN-CODE > 0 THEN                                              
001990        MOVE RCODE-16 TO RCODE                                            
002000        PERFORM Z-FINIT                                                   
002010        GOBACK                                                            
002020     END-IF                                                               
002030D    DISPLAY INFILE-V161002                                               
002040     CONTINUE.                                                            
002050     EJECT                                                                
002051                                                                          
002052*-----------------------------------------------------------------        
002060 S10-WRITE-OUTFILE SECTION.                                               
002061*-----------------------------------------------------------------        
002070     MOVE 'S10-WRITE-OUTFILE          ' TO ABEND-SECTION                  
002080D    DISPLAY ABEND-SECTION                                                
002090     SKIP2                                                                
002100D    DISPLAY OUTFILE-V161003                                              
002110     WRITE OUTFILE-POST FROM OUTFILE-V161003                              
002120     IF RETURN-CODE > 0 THEN                                              
002130        MOVE RCODE-16 TO RCODE                                            
002140        PERFORM Z-FINIT                                                   
002150        GOBACK                                                            
002160     END-IF                                                               
002170     CONTINUE.                                                            
002180     EJECT                                                                
002181                                                                          
002182*-----------------------------------------------------------------        
002190 S15-CLOSE-INFILE SECTION.                                                
002191*-----------------------------------------------------------------        
002200     MOVE 'S15-CLOSE-INFILE          ' TO ABEND-SECTION                   
002210D    DISPLAY ABEND-SECTION                                                
002220     SKIP2                                                                
002230     CLOSE INFILE                                                         
002240     IF RETURN-CODE = 0 THEN                                              
002250        MOVE NOO TO SW-INFILE-OPEN                                        
002260     ELSE                                                                 
002270        MOVE NOO TO SW-INFILE-OPEN                                        
002280        MOVE RCODE-16 TO RCODE                                            
002290        PERFORM Z-FINIT                                                   
002300        GOBACK                                                            
002310     END-IF                                                               
002320     CONTINUE.                                                            
002330     EJECT                                                                
002331                                                                          
002332*-----------------------------------------------------------------        
002340 S15-CLOSE-OUTFILE SECTION.                                               
002341*-----------------------------------------------------------------        
002350     MOVE 'S15-CLOSE-OUTFILE          ' TO ABEND-SECTION                  
002360D    DISPLAY ABEND-SECTION                                                
002370     SKIP2                                                                
002380     CLOSE OUTFILE                                                        
002390     IF RETURN-CODE = 0 THEN                                              
002400        MOVE NOO TO SW-OUTFILE-OPEN                                       
002410     ELSE                                                                 
002420        MOVE NOO TO SW-OUTFILE-OPEN                                       
002430        MOVE RCODE-16 TO RCODE                                            
002440        PERFORM Z-FINIT                                                   
002450        GOBACK                                                            
002460     END-IF                                                               
002470     CONTINUE.                                                            
002480     EJECT                                                                
002481                                                                          
002482*-----------------------------------------------------------------        
002490 T15-SELECT-EXTFILE SECTION.                                              
002491*-----------------------------------------------------------------        
002500     MOVE 'T15-SELECT-EXTFILE        ' TO ABEND-SECTION                   
002510D    DISPLAY ABEND-SECTION                                                
002520     SKIP2                                                                
002530     EXEC SQL                                                             
002540       SELECT A.EXTRACT_FILE                                              
002550       INTO  :EXTRACT-EXTRACT-FILE                                        
002570       FROM   EXTRACT A,                                                  
002580              EXTRACT_USER B                                              
002590       WHERE  A.EXTRACTID = B.EXTRACTID                                   
002600       AND    B.EXTRACTID = :EXTRACT-EXTRACTID                            
002610       AND    B.USERID    = USER                                          
002620     END-EXEC                                                             
002621     IF SQLCODE = +000 THEN                                               
002622       MOVE YES TO SW-EXTRACT-FOUND                                       
002623     ELSE                                                                 
002624       MOVE NOO TO SW-EXTRACT-FOUND                                       
002625     END-IF                                                               
002630     PERFORM S95-CONTROL-SQLCODE                                          
002740     CONTINUE.                                                            
002750     EJECT                                                                
002760*-------------------------------- DB2 ERROR HANDLING                      
002770*    -COPY V161PS                                                         
002780*++INCLUDE V161PS                                                         
002790                                                                          
