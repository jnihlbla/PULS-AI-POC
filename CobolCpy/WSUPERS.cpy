000010*** EDIT ALLOWED                                                          
000100 01  SUPERSESSION.                                                        
000200     03  IDARTNR                 PIC S9(9)         COMP-3.                
000300*                         *** PART NUMBER REPLACED                        
000400     03  REKSIFFR                PIC S9(1)         COMP-3.                
000500*                         *** CHECK DIGIT                                 
000600     03  DIERS-ERS               PIC S9(4)V9(3)    COMP-3.                
000700*                         *** QUANTITY REPLACED                           
000800     03  TIERSDAT                PIC S9(5)         COMP-3.                
000900*                         *** SUPERSESSION DATE                           
000901     03  KDERS                   PIC S9(3)         COMP-3.                
000902*                         *** SUPERSESSION CODE                           
000910     03  IDKORTNR                PIC S9(3)         COMP-3.                
000920*                         *** CARD NUMBER FOR SUPERSESSION INFO           
001000     03  FLTEXT                  PIC X(1).                                
001100*                         *** IF FLTEXT = J TYP2 INFORMATION              
001200*                         ***       ELSE    TYP1 INFORMATION              
001300     03  TYP1.                                                            
001400         05  IDARTNR-TILLK       PIC S9(9)         COMP-3.                
001500*                         *** REPLACING PART NUMBER                       
001600         05  DIERS-TILLK         PIC S9(4)V9(3)    COMP-3.                
001700*                         *** REPLACING QUATITY                           
001800         05  REKSIFFR-TILLK      PIC S9(1)         COMP-3.                
001900*                         *** REPLACING CHECK DIGIT                       
002000         05  FILLER           PIC X(10).                                  
002100     03  TYP2 REDEFINES TYP1.                                             
002200         05  BEERS               PIC X(20).                               
002300*                         *** REPLACING TEXT                              
