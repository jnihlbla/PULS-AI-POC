000010*** EDIT ALLOWED                                                          
000100*                      ***************************************            
000200*                      *** ANVÄNDS VID TEST AV:                           
      *                      ***  MOTTAGANDE LAND FRÅN DC21                     
000300*                      ***  - DISTRIKT SOM TILLHÖR BELGIEN                
000310*                      ***  - DISTRIKT SOM TILLHÖR NEDERLÄNDERNA          
000310*                      ***  - DISTRIKT SOM TILLHÖR TYSKLAND               
000310*                      ***  - DISTRIKT SOM TILLHÖR SCHWEIZ                
000400*                      ***                                                
000500*                      ***************************************            
000600                                                                          
000700 01  DIST89-IDDISTR          PIC 9(5)     COMP-3.                         
000800*                                                                         
001210       88  DIST89-BELGIEN             VALUE 1257 THRU 1258                
                                                  1274 THRU 1275                
                                                  1279 THRU 1282                
                                                  1287                          
                                                  1291 THRU 1292.               
001212       88  DIST89-HOLLAND             VALUE 1619 THRU 1622                
                                                  1624                          
                                                  1627 THRU 1628                
                                                  1678 THRU 1679.               
001214       88  DIST89-TYSKLAND            VALUE 2259 2270 2278.               
001215       88  DIST89-SCHWEIZ             VALUE 2059 2078.                    
001231*                                                                         
001400*** END COPY WWDIST89    LENGTH=3                                         
