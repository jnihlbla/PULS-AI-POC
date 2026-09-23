000100 01  REQU-WL0134I1.                                                       
000200*                                 REQUEST TO PGM WL0134                   
000300     03 REQU-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 REQU-IDPRC-KEY.                                                   
000600*                                 PRODUKTIONSKANAL                        
000700        05 REQU-IDPRCBAS     PIC X(3).                                    
000800*                                 PRC-BAS                                 
000900        05 REQU-IDPRCVAR     PIC X.                                       
001000*                                 PRC-VARIANT                             
001100     03 REQU-IDTRP-KEY.                                                   
001200*                                 TRANSPORTIDENTITET                      
001300        05 REQU-IDTRPLOS     PIC X(3).                                    
001400*                                 TRANSPORTLÖSNING                        
001500        05 REQU-IDTRPVAR     PIC X(2).                                    
001600*                                 TRANSPORTLÖSNINGSGRUPP                  
001700     03 REQU-IDANSTNR        PIC 9(5).                                    
001800*                                 ANSTÄLLNINGSNUMMER                      
001900     03 REQU-IDBORD          PIC X(3).                                    
002000*                                 PACK-BORD                               
002100     03 REQU-KDMATT          PIC X.                                       
002200*                                 MÅTTKOD                                 
002300     03 REQU-KVRADER-MAX1    PIC 9(5).                                    
002400*                                 MAX INDEX KOPPLAT TILL OCCURS N         
002500*                                 EDAN.                                   
002600     03 REQU-IDTRANS         PIC X(4).                                    
002700*                                 BILDNUMMER                              
002800     03 REQU-TABELLRAD       OCCURS 1 TO 1100 TIMES                       
002900                             DEPENDING ON REQU-KVRADER-MAX1.              
003000*                                 GRUPP MED RADER TILL ORDERDEL           
003100        05 REQU-FLORDDEL     PIC X.                                       
003200*                                 ALLMÄN FLAGGA                           
003300        05 REQU-IDPRODNR     PIC 9(7).                                    
003400*                                 PRODUKTIONSNUMMER                       
003500        05 REQU-IDPLKLST     PIC 9(3).                                    
003600*                                 PLOCKLISTNUMMER                         
003700*** END OF VILMAII-COPY LENGTH= 12129 BYTES                               
