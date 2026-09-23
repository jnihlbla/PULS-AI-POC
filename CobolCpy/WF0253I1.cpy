000100 01  REQU-WF0253I1.                                                       
000200*                                 REQUEST-COPYTEXT PGM WF0253             
000300*                                 SYSTEM MAINTENANCE                      
000400     03 REQU-IDSYSTEM-KEY    PIC X(4).                                    
000500*                                 VOLVO VCCS SYSTEMNUMMER                 
000600*                                 VOLVO VCCS SYSTEM NUMBER                
000700     03 REQU-KVMINUT         PIC 9(5).                                    
000800*                                 ANTAL MINUTER                           
000900*                                 NO OF MINUTES                           
001000     03 REQU-KVDAGAR         PIC 9(2).                                    
001100*                                 ANTAL DAGAR                             
001200     03 REQU-FLKLAR          PIC X.                                       
001300*                                 AVSLUTNINGSMARKERING                    
001400*                                 FINISHED FLAG                           
001500     03 REQU-DAREGDAT        PIC X(8).                                    
001600*                                 REGISTRERINGSDATUM (電電MMDD)           
001700*                                 REGISTRATION DATE (YYYYMMDD)            
001800     03 REQU-DAUPPDAT        PIC X(8).                                    
001900*                                 UPPDATERINGSDATUM  (電電MMDD)           
002000*                                                                         
002100*                                 UPDATING DATE     (YYYYMMDD)            
002200*                                                                         
002300*** END OF VILMAII-COPY LENGTH= 28 BYTES                                  
