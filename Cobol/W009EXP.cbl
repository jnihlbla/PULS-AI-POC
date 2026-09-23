000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W009EXP                                                  
000400*AUTHOR.         KJELL ANDRE.                                             
000500*DATE-WRITTEN.   95/04/12.                                                
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        BERÄKNAR EXPONENT. ANVÄNDS I EPLUS-PROGRAM                       
001000*                                                                         
001600                                                                          
002600 DATA DIVISION.                                                           
003100 WORKING-STORAGE SECTION.                                                 
003200                                                                          
003210                                                                          
003300*    -- CHECKED BY WY2000                                                 
006100     EJECT                                                                
006200 LINKAGE SECTION.                                                         
006300                                                                          
006400 01  ARG                         PIC S9(9)V9(3)  COMP-3.                  
006410 01  EXPO                        PIC S9(2)V9(3)  COMP-3.                  
006420 01  RES                         PIC S9(10)V9(3) COMP-3.                  
006700     EJECT                                                                
006800 PROCEDURE DIVISION USING ARG EXPO RES.                                   
007000     SKIP2                                                                
007100     COMPUTE RES = ARG ** EXPO                                            
007200     GOBACK.                                                              
