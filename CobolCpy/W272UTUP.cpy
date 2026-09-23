000100 01  UTUP-W272UTUP.                                                       
000200*                                 UPDATE UTILITY PROGRAM FOR REFI         
000300*                                 LL                                      
000400     03 UTUP-IN-UTDATA.                                                   
000500        05 UTUP-INDATA.                                                   
000600           07 UTUP-KDCALL    PIC S9(3)           COMP-3.                  
000700*                                 ANROPSTYP                               
000800*                                 CALL TYPE                               
000900           07 UTUP-IDARTNR   PIC S9(9)           COMP-3.                  
001000*                                 ARTIKELNUMMER                           
001100*                                 PART NUMBER                             
001200           07 UTUP-IDDC      PIC X(2).                                    
001300*                                 IDENTIFIERARE LAGER                     
001400*                                 WAREHOUSE IDENTIFIER                    
001500           07 UTUP-IDDC-REF  PIC X(2).                                    
001600*                                 SÄNDANDE LAGER FÖR REFILL               
001700*                                 SENDING WAREHOUSE FOR REFILL            
001800           07 UTUP-FILLER    PIC X(100).                                  
001900        05 UTUP-UTDATA.                                                   
002000           07 UTUP-KDSVAR    PIC X.                                       
002100            88 UTUP-KDSVAR-OK                                             
002200                             VALUE ' '.                                   
002300            88 UTUP-KDSVAR-FEL                                            
002400                             VALUE 'F'.                                   
002500*                                                       KDSVAR-88         
002600*                                 SVARSKOD FRÅN SUBPROGRAM                
002700*                                                       KDSVAR-88         
002800*                                 RETURN CODE FROM SUBPROGRAM             
002900           07 UTUP-TEXT      PIC X(25).                                   
003000           07 UTUP-LEADTID-BEHOV                                          
003100                             PIC S9(7)V9(2).                              
003200           07 UTUP-FILLER    PIC X(100).                                  
003300*** END OF VILMAII-COPY LENGTH= 246 BYTES                                 
