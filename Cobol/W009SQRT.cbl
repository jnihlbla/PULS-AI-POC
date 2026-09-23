000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W009SQRT                                                 
000400*AUTHOR.         STEFANO GIOBBI   -  *HAKUNA MATATA*                      
000500*DATE-WRITTEN.   96/07/04.                                                
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        BERÄKNAR SKURT. ANVÄNDS I EPLUS-PROGRAM                          
001000*                                                                         
001100                                                                          
001200 DATA DIVISION.                                                           
001300 WORKING-STORAGE SECTION.                                                 
001400                                                                          
001401                                                                          
001410*    -- CHECKED BY WY2000                                                 
001500     EJECT                                                                
001600 LINKAGE SECTION.                                                         
001700                                                                          
001800 01  ARG                         PIC S9(10)V9(3)  COMP-3.                 
001900 01  RES                         PIC S9(05)V9(8)  COMP-3.                 
002000     EJECT                                                                
002100 PROCEDURE DIVISION USING ARG RES.                                        
002200     SKIP2                                                                
002300     COMPUTE RES = FUNCTION SQRT (ARG)                                    
002400     GOBACK.                                                              
