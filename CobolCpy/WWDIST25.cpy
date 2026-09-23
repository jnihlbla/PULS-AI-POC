000010*** EDIT ALLOWED                                                          
000100*                                *********************************        
000200*                                *  ANVÄNDS VID BYTE AV FRAKTKOD *        
000300*                                *  PÅ AUSTRALIEN, PERU, USA,    *        
000400*                                *  JAPAN OCH ISRAEL             *        
000600*                                *********************************        
000700                                                                          
000800 01  DIST25-IDDISTR              PIC  9(5)     COMP-3.                    
000900*                                                                         
001000   88  DIST25-AUSTRAL-FRAKT      VALUE    7835 7836.                      
001100   88  DIST25-PERU-FRAKT         VALUE    6785.                           
001200   88  DIST25-JAPAN-FRAKT        VALUE    5220.                           
001300   88  DIST25-USA-FRAKT          VALUE    7512 7515 7531                  
001400                                          7552 7620 7625.                 
001410   88  DIST25-ISRAEL-FRAKT       VALUE    5120.                           
001500*** END COPY WWDIST25    LENGTH=3                                         
