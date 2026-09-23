000100 01  MID-W6I20801.                                                        
000200*                                 COPYTEXT FÖR MID W6I20801               
000300*                                                                         
000400     03 MID-IDKR-IN          PIC 9(5).                                    
000500*                                 KONTROLLRAPPORT NUMMER                  
000600     03 MID-IDKR-UT          PIC 9(5).                                    
000700*                                 KONTROLLRAPPORT NUMMER                  
000800     03 MID-INPUT.                                                        
000900*                                                                         
001000        05 MID-KDPERSON      PIC X(3).                                    
001100*                                 PERSONKOD                               
001200        05 MID-SUMMOR.                                                    
001300*                                                                         
001400           07 MID-SUOMK-INT-2DEC                                          
001500                             PIC X(10).                                   
001600*                                 SUMMA OMKOSTNADER                       
001700           07 MID-FLALL-SUOMK-INT                                         
001800                             PIC X.                                       
001900*                                 HELA BELOPPET VALT                      
002000           07 MID-SUOMK-EXT-2DEC                                          
002100                             PIC X(10).                                   
002200*                                 SUMMA OMKOSTNADER                       
002300           07 MID-FLALL-SUOMK-EXT                                         
002400                             PIC X.                                       
002500*                                 HELA BELOPPET VALT                      
002600           07 MID-SUMAT      PIC X(10).                                   
002700*                                 MATERIALKOSTNAD                         
002800           07 MID-FLALL-SUMAT                                             
002900                             PIC X.                                       
003000*                                 HELA BELOPPET VALT                      
003100        05 MID-TEKREKON-INT  PIC X(70).                                   
003200*                                 NOTERING EKONOMI INTERN                 
003300        05 MID-TEKREKON-EXT  PIC X(70).                                   
003400*                                 NOTERING EKONOMI EXTERN                 
003500        05 MID-TEKREKON      PIC X(70).                                   
003600*                                 NOTERING EKONOMI                        
003700*                                                                         
003800*** END OF VILMAII-COPY LENGTH= 256 BYTES                                 
