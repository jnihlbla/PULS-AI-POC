000010***************************************************************           
000020 ID  DIVISION.                                                            
000030***************************************************************           
000040 PROGRAM-ID.    V1611000.                                                 
000050 AUTHOR.        CARINA HOLMQVIST                                          
000060 DATE-WRITTEN.  SEPTEMBER 1991                                            
000070***************************************************************           
000080*                                                                         
000090*REMARKS:                                                                 
000100*                                                                         
000110*  * RETURNS ON OUTFILE EXTRACTS VALID FOR CURRENT USER                   
000130*                                                                         
000170*                                                                         
000180*  * SUBPROGRAMS:                                                         
000190*                                                                         
000210*        V16196 -   RETURNS CREATION DATE FOR EXTRACT-FILE                
000211*        V1611010 - RETURNS EXTRACTID WITH DESCRIPTION                    
000220*                                                                         
000230*  * OUTFILE DESCRIPTION:                                                 
000240*                                                                         
000250*        EXTRACTID,DESCRIPTION,SIZE,                                      
000260*        LAST-UPDATED,VALID-TO-DATE,AUTH                                  
000270*                                                                         
000280*  * RETURNCODES:                                                         
000290*                                                                         
000300*         4 - NO VALID EXTRACT REGISTERED FOR CURRENT USER                
000310*        16 - SEVERE FILE-ERROR                                           
000311*        20 - SEVERE DB2-ERROR                                            
000320*                                                                         
000330***************************************************************           
000340     EJECT                                                                
000350***************************************************************           
000360 ENVIRONMENT DIVISION.                                                    
000370***************************************************************           
000380     SKIP2                                                                
000390*--------------------------------------------------------------           
000400 CONFIGURATION SECTION.                                                   
000410*--------------------------------------------------------------           
000420 SOURCE-COMPUTER. IBM-370.                                                
000430*SOURCE-COMPUTER. IBM-370 WITH DEBUGGING MODE.                            
000440     SKIP2                                                                
000450*--------------------------------------------------------------           
000460 INPUT-OUTPUT SECTION.                                                    
000470*--------------------------------------------------------------           
000480 FILE-CONTROL.                                                            
000490     SELECT OUTFILE ASSIGN TO V16110D1.                                   
000500     EJECT                                                                
000510***************************************************************           
000520 DATA DIVISION.                                                           
000530***************************************************************           
000540     SKIP2                                                                
000550*--------------------------------------------------------------           
000560 FILE SECTION.                                                            
000570*--------------------------------------------------------------           
000580 FD  OUTFILE                                                              
000590     BLOCK CONTAINS 0                                                     
000600     RECORDING F                                                          
000610     LABEL RECORD STANDARD.                                               
000620 01  OUTFILE-POST                 PIC X(80).                              
000630     SKIP2                                                                
000640*--------------------------------------------------------------           
000650 WORKING-STORAGE SECTION.                                                 
000660*--------------------------------------------------------------           
000670 01  PROGRAM-NAME                 PIC X(8)  VALUE 'V1611000'.             
000680 01  ABEND-SECTION                PIC X(25) VALUE SPACE.                  
000690 01  RETURN-CODES.                                                        
000700     03  RCODE                    PIC S9(4) COMP SYNC VALUE ZERO.         
000710     03  RCODE-4                  PIC S9(4) COMP SYNC VALUE 4.            
000720     03  RCODE-8                  PIC S9(4) COMP SYNC VALUE 8.            
000730     03  RCODE-16                 PIC S9(4) COMP SYNC VALUE 16.           
000740     03  RCODE-20                 PIC S9(4) COMP SYNC VALUE 20.           
000750     SKIP2                                                                
000760 01  GENERAL-CONSTANTS.                                                   
000770     03 YES                       PIC X(1)  VALUE 'Y'.                    
000780     03 NOO                       PIC X(1)  VALUE 'N'.                    
000790     SKIP2                                                                
000800 01  SWITCHES.                                                            
000810     03 SW-VALID-NO-DAYS-MISSING  PIC X(1)  VALUE 'N'.                    
000811     03 SW-EXTRACT-FOUND          PIC X(1)  VALUE 'N'.                    
000820     SKIP2                                                                
000880 01  DYNAMIC-SUBPROGRAMS.                                                 
000902     03 V16196                    PIC X(8)  VALUE 'V16196  '.             
000903     03 V1611010                  PIC X(8)  VALUE 'V1611010'.             
000910     SKIP2                                                                
000984 01  V16196-PARMS.                                                        
000985     03 V16196-DSNAME             PIC X(44) VALUE SPACE.                  
000986     03 V16196-VALID-NO-DAYS      PIC 999   VALUE ZERO.                   
000987     03 V16196-LAST-UPDATED       PIC X(6)  VALUE SPACE.                  
000988     03 V16196-VALID-TO-DATE      PIC X(6)  VALUE SPACE.                  
000990     EJECT                                                                
001000*-------------------------------- COPY TEXT OUTFILE                       
001010 01  OUTFILE-V161001.                                                     
001020     03 OUTFILE-EXTRACTID          PIC X(8).                              
001030     03 OUTFILE-DESCRIPTION        PIC X(23).                             
001040     03 OUTFILE-SIZE               PIC ZZZZ9.                             
001050     03 OUTFILE-LAST-UPDATED       PIC X(6).                              
001060     03 OUTFILE-VALID-TO-DATE      PIC X(6).                              
001070     03 OUTFILE-AUTH               PIC X(1).                              
001080     03 OUTFILE-FILLER             PIC X(31).                             
001270     EJECT                                                                
001272                                                                          
001273 01  PARM-CODE                    PIC X(1).                               
001274*    -COPY V16110CC -PRE PARM-                                            
001275*++INCLUDE  V16110CC                                                      
001276                                                                          
001318     EJECT                                                                
001319*************************************************************             
001320 PROCEDURE DIVISION.                                                      
001321*************************************************************             
001323                                                                          
001324     PERFORM A-INIT                                                       
001325                                                                          
001330     CALL V1611010                                                        
001331     USING SW-EXTRACT-FOUND, PARM-CODE, PARM-V16110                       
001332                                                                          
001333     PERFORM UNTIL PARM-CODE = HIGH-VALUE                                 
001334                                                                          
001335        PERFORM  B-WRITE-OUTFILE                                          
001336                                                                          
001337        CALL V1611010                                                     
001338        USING SW-EXTRACT-FOUND, PARM-CODE, PARM-V16110                    
001339                                                                          
001340     END-PERFORM                                                          
001350                                                                          
001420     PERFORM Z-FINIT                                                      
001430     GOBACK                                                               
001440     CONTINUE.                                                            
001450                                                                          
001460*--------------------------------------------------------------           
001470 A-INIT SECTION.                                                          
001480*--------------------------------------------------------------           
001490     MOVE 'A-INIT                  ' TO ABEND-SECTION                     
001500D    DISPLAY ABEND-SECTION                                                
001510     SKIP2                                                                
001530     PERFORM S05-OPEN-OUTFILE                                             
001531                                                                          
001532     MOVE 'I' TO PARM-CODE                                                
001533                                                                          
001534     CALL V1611010                                                        
001535     USING SW-EXTRACT-FOUND, PARM-CODE, PARM-V16110                       
001536     MOVE SPACE TO PARM-CODE                                              
001537                                                                          
001540     CONTINUE.                                                            
001550     EJECT                                                                
001560*--------------------------------------------------------------           
001570 B-WRITE-OUTFILE SECTION.                                                 
001580*--------------------------------------------------------------           
001590     MOVE 'B-WRITE-OUTFILE           ' TO ABEND-SECTION                   
001600D    DISPLAY ABEND-SECTION                                                
001610     SKIP2                                                                
001620     PERFORM BA-INIT-OUTFILE-AREA                                         
001630     PERFORM S10-WRITE-OUTFILE                                            
001640     CONTINUE.                                                            
001650     EJECT                                                                
001660*--------------------------------------------------------------           
001670 BA-INIT-OUTFILE-AREA SECTION.                                            
001680*--------------------------------------------------------------           
001690     MOVE 'BA-EDIT-OUTFILE-AREA  ' TO ABEND-SECTION                       
001700D    DISPLAY ABEND-SECTION                                                
001710     SKIP2                                                                
001720     MOVE SPACE                     TO OUTFILE-V161001                    
001730     MOVE PARM-EXTRACTID            TO OUTFILE-EXTRACTID                  
001740     MOVE PARM-DESCRIPTION(1:23)    TO OUTFILE-DESCRIPTION                
001750     MOVE PARM-SIZE                 TO OUTFILE-SIZE                       
001761     MOVE PARM-EXTRACT-FILE         TO V16196-DSNAME                      
001762     MOVE PARM-VALID-NO-DAYS        TO V16196-VALID-NO-DAYS               
001763     MOVE SPACE                     TO V16196-LAST-UPDATED                
001764     MOVE SPACE                     TO V16196-VALID-TO-DATE               
001765                                                                          
001772                                                                          
001773     CALL V16196 USING V16196-DSNAME                                      
001774                       V16196-VALID-NO-DAYS                               
001775                       V16196-LAST-UPDATED                                
001776                       V16196-VALID-TO-DATE                               
001777     IF RETURN-CODE = 0 THEN                                              
001778        MOVE V16196-LAST-UPDATED    TO OUTFILE-LAST-UPDATED               
001779        MOVE V16196-VALID-TO-DATE   TO OUTFILE-VALID-TO-DATE              
001800     END-IF                                                               
001930                                                                          
001931     IF PARM-AUTH = 'A'                                                   
001932        MOVE 'I' TO OUTFILE-AUTH                                          
001933     ELSE                                                                 
001934        MOVE PARM-AUTH                 TO OUTFILE-AUTH                    
001935     END-IF                                                               
001936                                                                          
001940     CONTINUE.                                                            
001950     EJECT                                                                
001960*--------------------------------------------------------------           
001970 Z-FINIT SECTION.                                                         
001980*--------------------------------------------------------------           
001990     MOVE 'Z-FINIT                  ' TO ABEND-SECTION                    
002000D    DISPLAY ABEND-SECTION                                                
002010     SKIP2                                                                
002020     PERFORM S15-CLOSE-OUTFILE                                            
002030     MOVE RCODE TO RETURN-CODE                                            
002040     CONTINUE.                                                            
002050     EJECT                                                                
002060*--------------------------------------------------------------           
002070 S05-OPEN-OUTFILE SECTION.                                                
002080*--------------------------------------------------------------           
002090     MOVE 'S05-OPEN-OUTFILE           ' TO ABEND-SECTION                  
002100D    DISPLAY ABEND-SECTION                                                
002110     SKIP2                                                                
002120     OPEN OUTPUT OUTFILE                                                  
002130     IF RETURN-CODE > 0 THEN                                              
002140        MOVE RCODE-16 TO RETURN-CODE                                      
002150        GOBACK                                                            
002160     END-IF                                                               
002170     CONTINUE.                                                            
002180     SKIP2                                                                
002190*--------------------------------------------------------------           
002200 S10-WRITE-OUTFILE SECTION.                                               
002210*--------------------------------------------------------------           
002220     MOVE 'S10-WRITE-OUTFILE          ' TO ABEND-SECTION                  
002230D    DISPLAY ABEND-SECTION                                                
002240     SKIP2                                                                
002250D    DISPLAY 'OUTFILE ' OUTFILE-V161001                                   
002251                                                                          
002260     WRITE OUTFILE-POST FROM OUTFILE-V161001                              
002270     IF RETURN-CODE > 0 THEN                                              
002280        PERFORM S15-CLOSE-OUTFILE                                         
002290        MOVE RCODE-16 TO RETURN-CODE                                      
002300        GOBACK                                                            
002310     END-IF                                                               
002320     CONTINUE.                                                            
002330     SKIP2                                                                
002340*--------------------------------------------------------------           
002350 S15-CLOSE-OUTFILE SECTION.                                               
002360*--------------------------------------------------------------           
002370     MOVE 'S15-CLOSE-OUTFILE          ' TO ABEND-SECTION                  
002380D    DISPLAY ABEND-SECTION                                                
002390     SKIP2                                                                
002400     CLOSE OUTFILE                                                        
002410     IF RETURN-CODE > 0 THEN                                              
002420        MOVE RCODE-16 TO RETURN-CODE                                      
002430        GOBACK                                                            
002440     END-IF                                                               
002450     CONTINUE.                                                            
002460     EJECT                                                                
