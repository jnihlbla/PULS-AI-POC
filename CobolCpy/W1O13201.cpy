000100 01  MOD-W1O13201.                                                        
000200*                                 MOD-COPYTEXT  BILD 1132                 
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDPSN-IN         PIC X(3).                                    
000800*                                 PROPER SHIPPING NAME                    
000900     03 MOD-IDSPRAK-IN       PIC X(2).                                    
001000*                                 2-STÄLLIG ISO SPRÅKKOD                  
001100     03 MOD-IDPSN-UT         PIC X(3).                                    
001200*                                 PROPER SHIPPING NAME                    
001300     03 MOD-IDSPRAK-UT       PIC X(2).                                    
001400*                                 2-STÄLLIG ISO SPRÅKKOD                  
001500     03 MOD-W1013201-100.                                                 
001600        05 MOD-GRUPP-DGR.                                                 
001700           07 MOD-GRUPP-DGR-BEPSN                                         
001800                             OCCURS 3 TIMES.                              
001900              09 MOD-BEPSN-DGR-ATTR                                       
002000                             PIC X(2).                                    
002100*                                 MFS ATTRIBUTFÄLT                        
002200              09 MOD-BEPSN-DGR                                            
002300                             PIC X(75).                                   
002400*                                 PROPER SHIPPING NAME                    
002500        05 MOD-GRUPP-IMDG.                                                
002600           07 MOD-GRUPP-IMDG-BEPSN                                        
002700                             OCCURS 3 TIMES.                              
002800              09 MOD-BEPSN-IMDG-ATTR                                      
002900                             PIC X(2).                                    
003000*                                 MFS ATTRIBUTFÄLT                        
003100              09 MOD-BEPSN-IMDG                                           
003200                             PIC X(75).                                   
003300*                                 PROPER SHIPPING NAME                    
003400        05 MOD-GRUPP-IMDG-SF.                                             
003500           07 MOD-GRUPP-IMDG-SF-BEPSN                                     
003600                             OCCURS 3 TIMES.                              
003700              09 MOD-BEPSN-IMDG-SF-ATTR                                   
003800                             PIC X(2).                                    
003900*                                 MFS ATTRIBUTFÄLT                        
004000              09 MOD-BEPSN-IMDG-SF                                        
004100                             PIC X(75).                                   
004200*                                 PROPER SHIPPING NAME                    
004300        05 MOD-GRUPP-ADR.                                                 
004400           07 MOD-GRUPP-ADR-BEPSN                                         
004500                             OCCURS 3 TIMES.                              
004600              09 MOD-BEPSN-ADR-ATTR                                       
004700                             PIC X(2).                                    
004800*                                 MFS ATTRIBUTFÄLT                        
004900              09 MOD-BEPSN-ADR                                            
005000                             PIC X(75).                                   
005100*                                 PROPER SHIPPING NAME                    
005200        05 MOD-GRUPP-NOT.                                                 
005300           07 MOD-GRUPP-TEPSNNOT                                          
005400                             OCCURS 3 TIMES.                              
005500              09 MOD-TEPSNNOT-ATTR                                        
005600                             PIC X(2).                                    
005700*                                 MFS ATTRIBUTFÄLT                        
005800              09 MOD-TEPSNNOT                                             
005900                             PIC X(75).                                   
006000*                                 PROPER SHIPPING NAME NOTERING           
006100     03 MOD-TEMFSINF         PIC X(55).                                   
006200*                                 INFORMATIONSMEDDELANDE                  
006300*** END OF VILMAII-COPY LENGTH= 1264 BYTES                                
