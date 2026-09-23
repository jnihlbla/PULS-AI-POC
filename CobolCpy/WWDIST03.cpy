000010*** EDIT ALLOWED                                                          
000100*                            *************************************        
000200*                            *** ANVÄNDS VID TEST AV:                     
000300*                            ***  - SVENSKA DISTRIKT                      
000400*                            ***  - NORSKA  DISTRIKT                      
000500*                            ***  - DANSKA  DISTRIKT                      
000600*                            ***  - SKANDINAVISKA DISTRIKT                
000610*                            ***  - LYNK&CO DISTRIKT                      
000620*                            ***    738 = DISTRIKT INOM SVERIGE           
000630*                            ***    OBS!  MEN SOM INTE ALLTID HAR         
000640*                            ***    OBS!  SAMMA FUNKTION SOM 778          
000700*                            *************************************        
000800                                                                          
000900 01  DIST03-IDDISTR          PIC 9(5)    COMP-3.                          
001000*                                                                         
001100       88  DIST03-SVERIGE      VALUE    1 THRU  799.                      
001101*                                                                         
001102       88  DIST03-SVERIGE-2    VALUE    1 THRU  737                       
001103                                      740 THRU  799.                      
001104*                                                                         
001105       88  DIST03-SVERIGE-EJ-778 VALUE  1 THRU  699.                      
001107*                                                                         
001108       88  DIST03-EJ-AUTFAK    VALUE    1 THRU  69                        
001109                                        75 77                             
001109                                        80 83 88                          
001110                                        92 93 99.                         
001111*                                                                         
001112       88  DIST03-AP-AS        VALUE    77 81 90.                         
001113*                                                                         
001114       88  DIST03-S            VALUE    81 90.                            
001115*                                                                         
001116       88  DIST03-AS           VALUE    70 THRU 76                        
001117                                        61 62 82                          
001118                                        71 81 90                          
001119                                        90 95 96 99.                      
001120*                                                                         
001121       88  DIST03-ITALIEN      VALUE    71 81 90.                         
001122*                                                                         
001123       88  DIST03-SVERIGE-100-799 VALUE  100 THRU  737                    
001124                                         740 THRU  799.                   
001130*                                                                         
001200       88  DIST03-NORGE        VALUE  800 THRU  899                       
001300                                     1130 THRU 1169.                      
001310*                                                                         
001400       88  DIST03-DANMARK      VALUE  900 THRU  999                       
001500                                     1170 THRU 1199.                      
001510*                                                                         
001600       88  DIST03-SKANDINAVIEN VALUE    1 THRU  999                       
001700                                     1130 THRU 1199.                      
001710*                                                                         
001800       88  DIST03-FINLAND      VALUE 1000 THRU 1099.                      
001801*                                                                         
001810       88  DIST03-HELA-NORDEN  VALUE    1 THRU 1199.                      
001820*                                                                         
001830       88  DIST03-DANMARK-900  VALUE  900 THRU  999.                      
001900*                                                                         
002000*** END COPY WWDIST03C0  LENGTH=3     OLD LENGTH=0                        
