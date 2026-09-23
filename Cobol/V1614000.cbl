000010*****************************************************************         
000020 ID  DIVISION.                                                            
000030*****************************************************************         
000040 PROGRAM-ID.    V1614000.                                                 
000050 AUTHOR.        CARINA HOLMQVIST                                          
000060 DATE-WRITTEN.  SEPTEMBER 1991                                            
000061***************************************************************           
000062*                                                                         
000063*REMARKS:                                                                 
000064*                                                                         
000065*  * RETURNS ON OUTFILE EXTRACT_JOB FOR EXTRACTIDS SPECIFIED ON           
000066*    INFILE                                                               
000067*                                                                         
000068*  * DB2-TABLES:                                                          
000069*                                                                         
000070*        EXTRACT,EXTRACT_USER                                             
000071*                                                                         
000072*  * SUBPROGRAMS:                                                         
000073*                                                                         
000074*        V16195 - RETURNS LOGONID FOR CURRENT USER                        
000075*                                                                         
000082*                                                                         
000083*  * INFILE:                                                              
000084*                                                                         
000085*        CONTAINS A LIST OF EXTRACTIDS                                    
000087*                                                                         
000088*  * OUTFILE:                                                             
000089*                                                                         
000090*        EXTRACTID EXTRACT_JOB                                            
000092*                                                                         
000093*  * RETURNCODES:                                                         
000094*                                                                         
000095*         4 - NO AUTHORIZATION FOR EXTRACTID                              
000096*        16 - SEVERE FILE-ERROR                                           
000097*        20 - SEVERE DB2-ERROR                                            
000098*                                                                         
000099***************************************************************           
000100                                                                          
000101*****************************************************************         
000102 ENVIRONMENT DIVISION.                                                    
000103*****************************************************************         
000110 CONFIGURATION SECTION.                                                   
000120 SOURCE-COMPUTER. IBM-370.                                                
000130*SOURCE-COMPUTER. IBM-370 WITH DEBUGGING MODE.                            
000140                                                                          
000150 INPUT-OUTPUT SECTION.                                                    
000160 FILE-CONTROL.                                                            
000170     SELECT INFILE ASSIGN  TO V16140D1.                                   
000180     SELECT OUTFILE ASSIGN TO V16140D2.                                   
000190                                                                          
000200*****************************************************************         
000210 DATA DIVISION.                                                           
000220*****************************************************************         
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
000350                                                                          
000360 WORKING-STORAGE SECTION.                                                 
000370 01  PROGRAM-NAME                 PIC X(8)  VALUE 'V1614000'.             
000380 01  ABEND-SECTION                PIC X(25) VALUE SPACE.                  
000390 01  SWITCHES.                                                            
000400     03  SW-EOF-INFILE            PIC X(1)  VALUE 'N'.                    
000410     03  SW-INFILE-OPEN           PIC X(1)  VALUE 'N'.                    
000420     03  SW-OUTFILE-OPEN          PIC X(1)  VALUE 'N'.                    
000440     03  SW-EXTRACT-JOB-FOUND     PIC X(1)  VALUE 'Y'.                    
000442*                                                                         
000450 01  RETURN-CODES.                                                        
000460     03  RCODE                    PIC S9(4) COMP SYNC VALUE ZERO.         
000470     03  RCODE-4                  PIC S9(4) COMP SYNC VALUE 4.            
000480     03  RCODE-8                  PIC S9(4) COMP SYNC VALUE 8.            
000490     03  RCODE-16                 PIC S9(4) COMP SYNC VALUE 16.           
000500     03  RCODE-20                 PIC S9(4) COMP SYNC VALUE 20.           
000510*                                                                         
000520 01  GENERAL-CONSTANS.                                                    
000530     03 YES                       PIC X(1)  VALUE 'Y'.                    
000540     03 NOO                       PIC X(1)  VALUE 'N'.                    
000550*                                                                         
000560 01  WS-VARIABLER.                                                        
000581     03 WS-USERID                 PIC X(7)  VALUE SPACE.                  
000590*                                                                         
000600 01  ERROR-MESSAGES.                                                      
000610     03 EXT01.                                                            
000620        05 FILLER                 PIC X(3)  VALUE '***'.                  
000630        05 MESSAGE-ID             PIC X(5)  VALUE 'EXT01'.                
000640        05 MESSAGE-TEXT           PIC X(25) VALUE                         
000650        'NOT AUTHORIZED           '.                                      
000710*                                                                         
000790     EJECT                                                                
000791 01  DYNAMIC-SUBPROGRAMS.                                                 
000792     03 V16195                    PIC X(8)  VALUE 'V16195  '.             
000793 01  VGETLID-PARMS.                                                       
000794     03 VGETLID-LENGTH            PIC S9(4) COMP VALUE ZERO.              
000795     03 VGETLID-LOGONID           PIC X(8)  VALUE SPACE.                  
000800*-------------------------------- COPYTEXT INFILE                         
000810 01  INFILE-V161002.                                                      
000820     10 INFILE-EXTRACTID          PIC X(8).                               
000830     10 INFILE-FILLER             PIC X(72).                              
000840     SKIP2                                                                
000850*-------------------------------- COPYTEXT OUTFILE                        
000860 01  OUTFILE-V161003.                                                     
000870     10 OUTFILE-EXTRACTID         PIC X(8).                               
000880     10 OUTFILE-EXTRACT-JOB       PIC X(52).                              
000890     10 OUTFILE-FILLER            PIC X(20).                              
000900     SKIP2                                                                
000910*-------------------------------- DB2-AREAS                               
000920     EXEC SQL                                                             
000930          INCLUDE SQLCA                                                   
000940     END-EXEC.                                                            
000950     EXEC SQL                                                             
000960          INCLUDE EXTRACT                                                 
000970     END-EXEC.                                                            
000980*    -COPY EXTRACT -PRE EXTRACT-                                          
001000     EXEC SQL                                                             
001010          INCLUDE EXTUSER                                                 
001020     END-EXEC.                                                            
001030*    -COPY EXTUSER -PRE EXTUSER-                                          
001050*-------------------------------- DB2 ERROR HANDLING                      
001060*    -COPY V161WS                                                         
001070*++INCLUDE V161WSCCC0                                                     
001080                                                                          
001090*************************************************************             
001100 PROCEDURE DIVISION.                                                      
001110*************************************************************             
001120     PERFORM A-INIT                                                       
001130     PERFORM S10-READ-INFILE                                              
001140     PERFORM UNTIL SW-EOF-INFILE = YES                                    
001160        MOVE INFILE-EXTRACTID TO EXTRACT-EXTRACTID                        
001170        PERFORM T15-SELECT-EXTJOB                                         
001190        IF SW-EXTRACT-JOB-FOUND = NOO THEN                                
001200          MOVE RCODE-4        TO RCODE                                    
001240        END-IF                                                            
001286        PERFORM  B-INIT-OUTFILE-AREA                                      
001290        PERFORM  S10-WRITE-OUTFILE                                        
001300        PERFORM  S10-READ-INFILE                                          
001310     END-PERFORM                                                          
001320     PERFORM Z-FINIT                                                      
001330     GOBACK                                                               
001340     CONTINUE.                                                            
001350     EJECT                                                                
001360                                                                          
001370*---------------------------------------------------------------*         
001380 A-INIT SECTION.                                                          
001390*---------------------------------------------------------------*         
001400     MOVE 'A-INIT                   ' TO ABEND-SECTION                    
001410D    DISPLAY ABEND-SECTION                                                
001420     SKIP2                                                                
001430                                                                          
001440     CALL V16195 USING VGETLID-LENGTH                                     
001441                         VGETLID-LOGONID                                  
001442     MOVE VGETLID-LOGONID TO WS-USERID                                    
001450     PERFORM S05-OPEN-INFILE                                              
001460     PERFORM S05-OPEN-OUTFILE                                             
001470     CONTINUE.                                                            
001480     EJECT                                                                
001490                                                                          
001491*---------------------------------------------------------------*         
001492 B-INIT-OUTFILE-AREA SECTION.                                             
001493*---------------------------------------------------------------*         
001494     MOVE 'B-INIT-OUTFILE-AREA      ' TO ABEND-SECTION                    
001495D    DISPLAY ABEND-SECTION                                                
001496     SKIP2                                                                
001497                                                                          
001498     MOVE SPACE            TO OUTFILE-V161003                             
001499     MOVE INFILE-EXTRACTID TO OUTFILE-EXTRACTID                           
001500     IF SW-EXTRACT-JOB-FOUND = YES THEN                                   
001501        MOVE EXTRACT-EXTRACT-JOB        TO OUTFILE-EXTRACT-JOB            
001502     ELSE                                                                 
001503        MOVE EXT01                         TO OUTFILE-EXTRACT-JOB         
001504     END-IF                                                               
001508     CONTINUE.                                                            
001509     EJECT                                                                
001510                                                                          
001511*---------------------------------------------------------------*         
001512 Z-FINIT SECTION.                                                         
001520*---------------------------------------------------------------*         
001530     MOVE 'Z-FINIT                  ' TO ABEND-SECTION                    
001540D    DISPLAY ABEND-SECTION                                                
001550     SKIP2                                                                
001560                                                                          
001570     IF SW-INFILE-OPEN = YES THEN                                         
001580        PERFORM S15-CLOSE-INFILE                                          
001590     END-IF                                                               
001600     IF SW-OUTFILE-OPEN = YES THEN                                        
001610        PERFORM S15-CLOSE-OUTFILE                                         
001620     END-IF                                                               
001630     MOVE RCODE TO RETURN-CODE                                            
001640     CONTINUE.                                                            
001650     EJECT                                                                
001660                                                                          
001670*---------------------------------------------------------------*         
001680 S05-OPEN-INFILE SECTION.                                                 
001690*---------------------------------------------------------------*         
001700     MOVE 'S05-OPEN-INFILE           ' TO ABEND-SECTION                   
001710D    DISPLAY ABEND-SECTION                                                
001720     SKIP2                                                                
001730                                                                          
001740     MOVE NOO TO SW-INFILE-OPEN                                           
001750     OPEN INPUT INFILE                                                    
001760     IF RETURN-CODE = 0 THEN                                              
001770        MOVE YES TO SW-INFILE-OPEN                                        
001780     ELSE                                                                 
001790        MOVE RCODE-16 TO RCODE                                            
001800        PERFORM Z-FINIT                                                   
001810        GOBACK                                                            
001820     END-IF                                                               
001830     CONTINUE.                                                            
001840     EJECT                                                                
001850                                                                          
001860*---------------------------------------------------------------*         
001870 S05-OPEN-OUTFILE SECTION.                                                
001880*---------------------------------------------------------------*         
001890     MOVE 'S05-OPEN-OUTFILE           ' TO ABEND-SECTION                  
001900D    DISPLAY ABEND-SECTION                                                
001910     SKIP2                                                                
001920                                                                          
001930     MOVE NOO TO SW-OUTFILE-OPEN                                          
001940     OPEN OUTPUT OUTFILE                                                  
001950     IF RETURN-CODE = 0 THEN                                              
001960        MOVE YES TO SW-OUTFILE-OPEN                                       
001970     ELSE                                                                 
001980        MOVE RCODE-16 TO RCODE                                            
001990        PERFORM Z-FINIT                                                   
002000        GOBACK                                                            
002010     END-IF                                                               
002020     CONTINUE.                                                            
002030     EJECT                                                                
002040                                                                          
002050*---------------------------------------------------------------*         
002060 S10-READ-INFILE SECTION.                                                 
002070*---------------------------------------------------------------*         
002080     MOVE 'S10-READ-INFILE           ' TO ABEND-SECTION                   
002090D    DISPLAY ABEND-SECTION                                                
002100     SKIP2                                                                
002110                                                                          
002120     READ                                                                 
002130        INFILE INTO INFILE-V161002                                        
002140        AT END MOVE YES TO SW-EOF-INFILE                                  
002150     END-READ                                                             
002160     IF RETURN-CODE > 0 THEN                                              
002170        MOVE RCODE-16 TO RCODE                                            
002180        PERFORM Z-FINIT                                                   
002190        GOBACK                                                            
002200     END-IF                                                               
002210D    DISPLAY INFILE-V161002                                               
002220     CONTINUE.                                                            
002230     EJECT                                                                
002240                                                                          
002250*---------------------------------------------------------------*         
002260 S10-WRITE-OUTFILE SECTION.                                               
002270*---------------------------------------------------------------*         
002280     MOVE 'S10-WRITE-OUTFILE          ' TO ABEND-SECTION                  
002290D    DISPLAY ABEND-SECTION                                                
002300     SKIP2                                                                
002310                                                                          
002320D    DISPLAY OUTFILE-V161003                                              
002330     WRITE OUTFILE-POST FROM OUTFILE-V161003                              
002340     IF RETURN-CODE > 0 THEN                                              
002350        MOVE RCODE-16 TO RCODE                                            
002360        PERFORM Z-FINIT                                                   
002370        GOBACK                                                            
002380     END-IF                                                               
002390     CONTINUE.                                                            
002400     EJECT                                                                
002410                                                                          
002420*---------------------------------------------------------------*         
002430 S15-CLOSE-INFILE SECTION.                                                
002440*---------------------------------------------------------------*         
002450     MOVE 'S15-CLOSE-INFILE          ' TO ABEND-SECTION                   
002460D    DISPLAY ABEND-SECTION                                                
002470     SKIP2                                                                
002480                                                                          
002490     CLOSE INFILE                                                         
002500     IF RETURN-CODE = 0 THEN                                              
002510        MOVE NOO TO SW-INFILE-OPEN                                        
002520     ELSE                                                                 
002530        MOVE NOO TO SW-INFILE-OPEN                                        
002540        MOVE RCODE-16 TO RCODE                                            
002550        PERFORM Z-FINIT                                                   
002560        GOBACK                                                            
002570     END-IF                                                               
002580     CONTINUE.                                                            
002590     EJECT                                                                
002600                                                                          
002610*---------------------------------------------------------------*         
002620 S15-CLOSE-OUTFILE SECTION.                                               
002630     MOVE 'S15-CLOSE-OUTFILE          ' TO ABEND-SECTION                  
002640D    DISPLAY ABEND-SECTION                                                
002650     SKIP2                                                                
002660                                                                          
002670     CLOSE OUTFILE                                                        
002680     IF RETURN-CODE = 0 THEN                                              
002690        MOVE NOO TO SW-OUTFILE-OPEN                                       
002700     ELSE                                                                 
002710        MOVE NOO TO SW-OUTFILE-OPEN                                       
002720        MOVE RCODE-16 TO RCODE                                            
002730        PERFORM Z-FINIT                                                   
002740        GOBACK                                                            
002750     END-IF                                                               
002760     CONTINUE.                                                            
002770     EJECT                                                                
002780                                                                          
002790*---------------------------------------------------------------*         
002800 T15-SELECT-EXTJOB SECTION.                                               
002810*---------------------------------------------------------------*         
002820     MOVE 'T15-SELECT-EXTJOB       ' TO ABEND-SECTION                     
002830D    DISPLAY ABEND-SECTION                                                
002840     SKIP2                                                                
002850                                                                          
002860     EXEC SQL                                                             
002870       SELECT A.EXTRACT_JOB                                               
002880       INTO  :EXTRACT-EXTRACT-JOB                                         
002890       FROM   EXTRACT A,                                                  
002900              EXTRACT_USER B                                              
002910       WHERE  A.EXTRACTID     = B.EXTRACTID                               
002920       AND    B.EXTRACTID     = :EXTRACT-EXTRACTID                        
002930       AND    B.USERID        = :WS-USERID                                
002940       AND    B.AUTH = 'I'                                                
002950     END-EXEC                                                             
002960     PERFORM S95-CONTROL-SQLCODE                                          
002970     IF SQLCODE = +000 THEN                                               
003020       MOVE YES  TO SW-EXTRACT-JOB-FOUND                                  
003040     ELSE                                                                 
003050       MOVE NOO TO SW-EXTRACT-JOB-FOUND                                   
003060     END-IF                                                               
003070     CONTINUE.                                                            
003080     EJECT                                                                
003090*-------------------------------- DB2 ERROR HANDLING                      
003100*    -COPY V161PS                                                         
003110*++INCLUDE V161PSCCC0                                                     
003120                                                                          
