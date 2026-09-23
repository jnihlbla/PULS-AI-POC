000100 01  2236-WDGX2236.                                                       
000200*                                 SATS                                    
000300*                                 FRÅN ANSK TILL SATSORDER                
000400*                                 FYSISK NYCKEL WDGXKEY:                  
000500*                                 IDARTNR, TIBEHOV, LOW-VALUE             
000600     03 2236-IDARTNR         PIC S9(9)           COMP-3.                  
000700*                                 ARTIKELNUMMER                           
000800*                                 PART NUMBER                             
000900     03 2236-TIBEHOV         PIC S9(5)           COMP-3.                  
001000*                                 BEHOVSVECKA           (ÅÅVV)            
001100     03 2236-LOW-VALUE       PIC X(2).                                    
001200     03 2236-IDANSK          PIC S9(3)           COMP-3.                  
001300*                                 ANSKAFFARNUMMER                         
001400*                                 PROCURER NO.                            
001500     03 2236-IDDISTR         PIC S9(5)           COMP-3.                  
001600*                                 DISTRIKTNUMMER                          
001700*                                 DISTRICT NUMBER                         
001800     03 2236-IDLEVNR         PIC X(5).                                    
001900*                                 LEVERANTÖRNUMMER                        
002000*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
002100     03 2236-KDCLAGER        PIC S9              COMP-3.                  
002200*                                 CENTRALLAGERKOD                         
002300*                                 CENTRAL WAREHOUSE CODE                  
002400     03 2236-KVBEART         PIC S9(7)           COMP-3.                  
002500*                                 BESTÄLLT ANTAL STYCKEN                  
002600*                                 ORDERED QUANTITY                        
002700     03 2236-FILLER          PIC X(17).                                   
002800*** END OF VILMAII-COPY LENGTH= 42 BYTES                                  
