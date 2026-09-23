000010*** EDIT ALLOWED                                                          
000100*                            *************************************        
000200*                            *** ANVÄNDS VID TEST AV:                     
000300*                            ***  - Distrikt som skall ha                 
000400*                            ***    speciell kurs.                        
000410*                            ***                                          
000420*                            ***    DIS121-BRASIL-NEW ANVÄNDS             
000430*                            ***    I W46120 OCH SKA EJ HA USD            
000500*                            ***    SOM VALUTA.                           
000600*                            *************************************        
000700                                                                          
000800 01  DIS121-IDDISTR     PIC  9(5)    COMP-3.                              
000900                                                                          
001000   88  DIS121-BRASIL         VALUE  7010 7020 7070 7170.                  
001010                                                                          
001011   88  DIS121-BRASIL-NEW     VALUE  7040 7050 7051 8152.                  
001012                                                                          
001013   88  DIS121-PERU           VALUE  6785.                                 
001014                                                                          
001020   88  DIS121-THAILAND       VALUE  6225 6251 8163.                       
001021                                                                          
001022   88  DIS121-SOUTH-AFRICA   VALUE  3160 3162.                            
001030*                                                                         
001040*** END COPY WWDIS121  LENGTH=3                                           
