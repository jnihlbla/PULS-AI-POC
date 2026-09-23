000100 01  4016-WDGX4016.                                                       
000200*                                 VIPS ORDERBEKRÄFTELSERADER              
000300*                                 FYSISK NYCKEL: KY4016                   
000400*                                 (IDARTNR + IDLOPNR +                    
000500*                                  IDSEKVNR + KDORDBEK                    
000600     03 4016-IDARTNR         PIC S9(9)           COMP-3.                  
000700*                                 ARTIKELNUMMER                           
000800*                                 PART NUMBER                             
000900     03 4016-IDLOPNR         PIC S9(3)           COMP-3.                  
001000*                                 LÖPNUMMER                               
001100*                                 SEQUENCE NUMBER                         
001200     03 4016-IDSEKVNR        PIC S9(3)           COMP-3.                  
001300*                                 GENERELLT SEKVENSNUMMER                 
001400*                                 GENERAL SEQUENCE NUMBER                 
001500     03 4016-KDORDBEK        PIC 9(2).                                    
001600*                                 ORDERBEKRÄFTELSEKOD                     
001700*                                 ORDERCONFIMATIONCODE                    
001800     03 4016-BEART-USA       PIC X(25).                                   
001900*                                 AMERIKANSK ART.BENÄMNING                
002000     03 4016-DAREGDAT        PIC 9(8).                                    
002100*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
002200*                                 REGISTRATION DATE (YYYYMMDD)            
002300     03 4016-KDORDKL         PIC S9              COMP-3.                  
002400*                                 ORDERKLASS                              
002500*                                 ORDER CLASS                             
002600     03 4016-KVBEART         PIC S9(7)           COMP-3.                  
002700*                                 BESTÄLLT ANTAL STYCKEN                  
002800*                                 ORDERED QUANTITY                        
002900*** END OF VILMAII-COPY LENGTH= 49 BYTES                                  
