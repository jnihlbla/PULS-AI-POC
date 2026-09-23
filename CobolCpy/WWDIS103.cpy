000010*** EDIT ALLOWED                                                          
000011*                            *************************************        
000012*                            *** ANVÄNDS VID TEST AV:                     
000013*                            *** DISTRIKT SOM SKALL RÄKNAS                
000014*                            *** MED I FÄRDIGPACKAT-FIL FÖR               
000015*                            *** SVERIGE KLASS 4, RETUREMB-               
000016*                            *** INFO TILL VTAB.                          
000017*                            *************************************        
000018*                                                                         
000019 01  DIS103-IDDISTR              PIC 9(5)    COMP-3.                      
000020*                                                                         
002030       88  DIS103-EMB-INFO-SE       VALUE   8700 THRU 8706.               
003000*                                                                         
002030       88  DIS103-EMB-INFO          VALUE   8700                          
002030                                            8701                          
002030                                            8703                          
002030                                            8704                          
002030                                            8706.                         
003000*                                                                         
005400*** END COPY WWDIS103  LENGTH=3                                           
