000010***************************************************************           
000020 ID  DIVISION.                                                            
000030***************************************************************           
000040 PROGRAM-ID.    V1611100.                                                 
000050 AUTHOR.        CARINA HOLMQVIST                                          
000060 DATE-WRITTEN.  SEPTEMBER 1991                                            
000070***************************************************************           
000080*                                                                         
000090*REMARKS:                                                                 
000100*                                                                         
000110*  * DRIVER FÖR V1611010                                                  
000130*                                                                         
000140*  * DB2-TABLES:                                                          
000150*                                                                         
000160*        EXTRACT,EXTRACT_USER                                             
000170*                                                                         
000180*  * SUBPROGRAMS:                                                         
000190*                                                                         
000210*        V16196 - RETURNS CREATION DATE FOR EXTRACT-FILE                  
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
000500                                                                          
000510***************************************************************           
000520 DATA DIVISION.                                                           
000530***************************************************************           
000540     SKIP2                                                                
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
000810     03 SW-ERROR                  PIC X(1)  VALUE 'N'.                    
000820     SKIP2                                                                
000880 01  DYNAMIC-SUBPROGRAMS.                                                 
000902     03 V1611010                  PIC X(8)  VALUE 'V1611010'.             
000910     SKIP2                                                                
000990                                                                          
001000*-------------------------------- COPY TEXT OUTFILE                       
001090     SKIP2                                                                
001091                                                                          
001092 01  PARM-CODE                    PIC X(1).                               
001093*    -COPY V16110CC -PRE PARM-                                            
001094*++INCLUDE  V16110CC                                                      
001095                                                                          
001270     EJECT                                                                
001280*************************************************************             
001290 PROCEDURE DIVISION.                                                      
001300*************************************************************             
001310                                                                          
001311     MOVE 'I' TO PARM-CODE                                                
001320     CALL V1611010 USING PARM-CODE, PARM-V16110                           
001330     MOVE SPACE TO PARM-CODE                                              
001340     CALL V1611010 USING PARM-CODE, PARM-V16110                           
001350     PERFORM UNTIL PARM-CODE = HIGH-VALUE                                 
001351                                                                          
001352        DISPLAY PARM-V16110                                               
001353                                                                          
001354        CALL V1611010 USING PARM-CODE, PARM-V16110                        
001355                                                                          
001360     END-PERFORM                                                          
001420     PERFORM Z-FINIT                                                      
001430     GOBACK                                                               
001440     CONTINUE.                                                            
001950     EJECT                                                                
001960*--------------------------------------------------------------           
001970 Z-FINIT SECTION.                                                         
001980*--------------------------------------------------------------           
001990     MOVE 'Z-FINIT                  ' TO ABEND-SECTION                    
002000D    DISPLAY ABEND-SECTION                                                
002010     SKIP2                                                                
002030     MOVE RCODE TO RETURN-CODE                                            
002040     CONTINUE.                                                            
002050     EJECT                                                                
