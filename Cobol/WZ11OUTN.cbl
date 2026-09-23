000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WZ11OUTN.                                                
000300 AUTHOR.         ANDRE KJELL.                                             
000400 DATE-WRITTEN.   02/12/20.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        THIS FUNCTIONALITY OF THIS PROGRAM HAS BEEN MOVED                
000900*        TO WZ11OUTM AND IT IS NO LONGER IN USE.                          
001000*                                                                         
001100*        CCID: 5173755  - MEMO BORT                                       
001200                                                                          
001300     EJECT                                                                
001400 DATA DIVISION.                                                           
001500     SKIP3                                                                
001600 WORKING-STORAGE SECTION.                                                 
001700 77  IDPGM                       PIC X(08)   VALUE 'WZ11OUTN'.            
001800     EJECT                                                                
001900 LINKAGE SECTION.                                                         
002000*01  -COPY  WZ11OUTM                                                      
002100     EJECT                                                                
002200 PROCEDURE DIVISION  USING OUTM-WZ11OUT.                                  
002300 MAIN SECTION.                                                            
002400                                                                          
002500     MOVE ZERO TO RETURN-CODE                                             
002600     GOBACK                                                               
002700     .                                                                    
