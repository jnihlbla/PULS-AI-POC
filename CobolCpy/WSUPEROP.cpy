000010*** EDIT ALLOWED                                                          
000100 01  SUPERSESSION.                                                        
000200     03  IDPTYP                  PIC X(3).                                
000300*                         *** RECORD TYPE ID                              
000310     03  IDARTNR                 PIC 9(9).                                
000320*                         *** PART NUMBER REPLACED                        
000400     03  REKSIFFR                PIC 9(1).                                
000500*                         *** CHECK DIGIT                                 
000600     03  DIERS-ERS               PIC 9(4)V9(3).                           
000700*                         *** QUANTITY REPLACED                           
000800     03  TIERSDAT                PIC 9(5).                                
000900*                         *** SUPERSESSION DATE                           
000901     03  KDERS                   PIC 9(3).                                
000902*                         *** SUPERSESSION CODE                           
000910     03  IDKORTNR                PIC 9(3).                                
000920*                         *** CARD NUMBER FOR SUPERSESSION INFO           
001000     03  FLTEXT                  PIC X(1).                                
001100*                         *** IF FLTEXT = J TYPE2 INFORMATION             
001200*                         ***       ELSE    TYPE1 INFORMATION             
001300     03  TYPE1.                                                           
001400         05  IDARTNR-TILLK       PIC 9(9).                                
001500*                         *** REPLACING PART NUMBER                       
001600         05  DIERS-TILLK         PIC 9(4)V9(3).                           
001700*                         *** REPLACING QUATITY                           
001800         05  REKSIFFR-TILLK      PIC 9(1).                                
001900*                         *** REPLACING CHECK DIGIT                       
002000         05  FILLER           PIC X(3).                                   
002100     03  TYPE2 REDEFINES TYPE1.                                           
002200         05  BEERS               PIC X(20).                               
002300*                         *** REPLACING TEXT                              
