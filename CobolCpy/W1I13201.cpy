000100 01  MID-W1I13201.                                                        
000200*                                 MID-COPYTEXT  BILD 1132                 
000300     03 MID-IDPSN-IN         PIC X(3).                                    
000400*                                 PROPER SHIPPING NAME                    
000500     03 MID-IDSPRAK-IN       PIC X(2).                                    
000600*                                 2-STÄLLIG ISO SPRÅKKOD                  
000700     03 MID-IDPSN-UT         PIC X(3).                                    
000800*                                 PROPER SHIPPING NAME                    
000900     03 MID-IDSPRAK-UT       PIC X(2).                                    
001000*                                 2-STÄLLIG ISO SPRÅKKOD                  
001100     03 MID-INPUT.                                                        
001200        05 MID-GRUPP-DGR.                                                 
001300           07 MID-GRP-DGR-BEPSN                                           
001400                             OCCURS 3 TIMES.                              
001500              09 MID-BEPSN-DGR                                            
001600                             PIC X(75).                                   
001700*                                 PROPER SHIPPING NAME                    
001800        05 MID-GRUPP-IMDG.                                                
001900           07 MID-GRP-IMDG-BEPSN                                          
002000                             OCCURS 3 TIMES.                              
002100              09 MID-BEPSN-IMDG                                           
002200                             PIC X(75).                                   
002300*                                 PROPER SHIPPING NAME                    
002400        05 MID-GRUPP-IMDG-SF.                                             
002500           07 MID-GRP-IMDG-SF-BEPSN                                       
002600                             OCCURS 3 TIMES.                              
002700              09 MID-BEPSN-IMDG-SF                                        
002800                             PIC X(75).                                   
002900*                                 PROPER SHIPPING NAME                    
003000        05 MID-GRUPP-ADR.                                                 
003100           07 MID-GRP-ADR-BEPSN                                           
003200                             OCCURS 3 TIMES.                              
003300              09 MID-BEPSN-ADR                                            
003400                             PIC X(75).                                   
003500*                                 PROPER SHIPPING NAME                    
003600        05 MID-GRUPP-NOT.                                                 
003700           07 MID-GRP-TEPSNNOT                                            
003800                             OCCURS 3 TIMES.                              
003900              09 MID-TEPSNNOT                                             
004000                             PIC X(75).                                   
004100*                                 PROPER SHIPPING NAME NOTERING           
004200*** END OF VILMAII-COPY LENGTH= 1135 BYTES                                
