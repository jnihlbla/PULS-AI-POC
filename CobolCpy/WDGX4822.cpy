000100 01  4822-WDGX4822.                                                       
000200*                                 FELTEXTER FÖR KVALITE                   
000300*                                 FYSISK NYCKEL                           
000400*                                 WDGXKEY =                               
000500*                                 (IDKVAFEL + LOW-VALUE)                  
000600     03 4822-IDKVAFEL        PIC 9(2).                                    
000700*                                 KVALITET FELKOD FÖR ARTIKEL             
000800*                                 ERROR CODE FOR PARTNUMBER               
000900     03 4822-LOW-VALUE       PIC X(3).                                    
001000     03 4822-KDKVAFG         PIC S9              COMP-3.                  
001100*                                 FELGRUPP FÖR FELKOD                     
001200*                                 ERROR GROUP FOR ERROR CODE              
001300     03 4822-KVKVAFPO        PIC S9(3)           COMP-3.                  
001400*                                 KVALITET POÄNG FÖR FELKOD               
001500*                                 QUALITY POINT FOR ERROR CODE            
001600     03 4822-FELTEXT-GRP     OCCURS 6 TIMES.                              
001700        05 4822-IDSKYLT      PIC X(3).                                    
001800*                                 NATIONALITETSTECKEN                     
001900*                                 SPRÅKIDENTIFIKATION                     
002000*                                 NATIONALITY SIGN                        
002100*                                 LANGUAGE IDENTIFIER                     
002200        05 4822-BEKVAFEL     PIC X(40).                                   
002300*                                 KVALITET FELKODSBETECKNING              
002400*                                 ERROR FOR ERROR CODE                    
002500        05 4822-BEKVAFGR     PIC X(20).                                   
002600*                                 KVALITET ALLVARLIGHETSGRAD FELK         
002700*                                 OD                                      
002800*                                 QUALITY SERIOUS FOR ERROR CODE          
002900*** END OF VILMAII-COPY LENGTH= 386 BYTES                                 
