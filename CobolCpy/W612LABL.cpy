000100 01  LABL-W612LABL.                                                       
000200*                                 COPYTEXT FOR                            
000300*                                 SUBPROGRAM W612LABL                     
000400     03 LABL-IDDC            PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600*                                 WAREHOUSE IDENTIFIER                    
000700     03 LABL-IDLIST          PIC X(10).                                   
000800*                                 LISTIDENTITET                           
000900*                                 LIST IDENTITY                           
001000     03 LABL-KVRADER         PIC 9(5).                                    
001100*                                 ANTAL RADER                             
001200*                                 NUMBER OF LINES                         
001300     03 LABL-PART-DATA       OCCURS 1100 TIMES.                           
001400*                                                                         
001500        05 LABL-IDARTNR      PIC S9(9)           COMP-3.                  
001600*                                 ARTIKELNUMMER                           
001700*                                 PART NUMBER                             
001800        05 LABL-KVANTAL      PIC S9(7)           COMP-3.                  
001900*                                 ANTAL                                   
002000*                                 NUMBER                                  
002100        05 LABL-KDARTURS     PIC X(2).                                    
002200*                                 ARTIKELURSPRUNGSKOD                     
002300*                                 COUNTRY OF ORIGIN                       
002400        05 LABL-KVQPACK      PIC S9(7)           COMP-3.                  
002500*                                 ANTAL                                   
002600*                                 NUMBER                                  
002700*** END OF VILMAII-COPY LENGTH= 16517 BYTES                               
