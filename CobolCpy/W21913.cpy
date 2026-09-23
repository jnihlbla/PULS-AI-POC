000100 01  W21913.                                                              
000200*                                 ARTIKLAR SOM INGÅR I KAMPANJER          
000300*                                 FRÅN QW90                               
000400*                                 POSTTYP = Q9A                           
000500*                                                                         
000600     03 IDPTYP               PIC X(3).                                    
000700*                                 POSTTYP                                 
000800     03 KDSTATUS-KAMP        PIC X.                                       
000900*                                 STATUS AV MATCHNING                     
001000*                                 N = NYTILLKOMMEN POST                   
001100*                                 C = FÖRÄNDRAD POST                      
001200*                                 D = BORTTAGEN POST                      
001300     03 IDKAMP               PIC X(7).                                    
001400*                                 SERVICEKAMPANJ                          
001500     03 IDARTNR              PIC 9(9).                                    
001600*                                 ARTIKELNUMMER                           
001700     03 KVREPANT             PIC 9(3)V9(2).                               
001800*                                 ANTAL PER REPARATION OCH BIL            
001900*** END OF VILMAII-COPY LENGTH= 25 BYTES                                  
