000010*** EDIT ALLOWED                                                          
000100*                            *************************************        
000200*                            *** ANVÄNDS VID TEST AV:                     
000300*                            ***  - VILKA USERID SOM SKALL STYRA          
000400*                            ***    DATA TILL RESPEKTIVE TULL-PC          
000410*                            ***    PC-1 STÅR PÅ NORDEN-KONTORET          
000420*                            ***    PC-2 STÅR PÅ RB-KONTORET.             
000430*                            ***         HIT STYRS ALLT SOM INTE          
000440*                            ***         FINNS DEFINIERAT                 
000500*                            ***                                          
000600*                            *************************************        
000700                                                                          
000800 01  USER01-IDUSER           PIC X(8).                                    
000900                                                                          
001030     88  USER01-PC2          VALUE 'PC00000 ' THRU 'PC16242 '             
001040                                   'PC16244 ' THRU 'PC29484 '             
001050                                   'PC29486 ' THRU 'PC35720 '             
001060                                   'PC35722 ' THRU 'PC51133 '             
001061                                   'PC51135 ' THRU 'PC51546 '             
001070                                   'PC51548 ' THRU 'PC54127 '             
001080                                   'PC54129 ' THRU 'PC54582 '             
001090                                   'PC54584 ' THRU 'PC54667 '             
001091                                   'PC54669 ' THRU 'PC56247 '             
001100                                   'PC56249 ' THRU 'PC57757 '             
001200                                   'PC57759 ' THRU 'PC58286 '             
001210                                   'PC58288 ' THRU 'PC59481 '             
001220                                   'PC59483 ' THRU 'PC60686 '             
001230                                   'PC60688 ' THRU 'PC63527 '             
001240                                   'PC63529 ' THRU 'PC64635 '             
001240                                   'PC64636 ' THRU 'PC87030 '             
001240                                   'PC87031 ' THRU 'PC99999 '             
001300                                   'PCCB302 '                             
001300                                   'PCREF01 '                             
001400                                   'PCPAC16 '.                            
001508                                                                          
001611*    88  USER01-PC1          VALUE 'PC59701 '.                            
001612     88  USER01-PC1          VALUE 'PCM3073 '                             
001613                                   'PC0FAKT '                             
001614                                   'PC16243 '                             
001614                                   'PC17901 '                             
001614                                   'PC20604 ' 'PC29485 '                  
001615                                   'PC35721 ' 'PC51134 '                  
001616                                   'PC51547 '                             
001617                                   'PC54128 ' 'PC54583 '                  
001618                                   'PC54668 '                             
001619                                   'PC56248 ' 'PC57758 '                  
001620                                   'PC58287 ' 'PC59482 '                  
001630                                   'PC60687 ' 'PC63528 '                  
001630                                   'PC64636 '                             
001630                                   'PC87031 '.                            
001720                                                                          
001800*** END COPY WWUSER01    LENGTH=8                                         
