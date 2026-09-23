000100 01  OHASH-W460003.                                                       
000200*                                 ORDER HASH-TOTAL NOAC                   
000300*                                 POSTTYP = RHC                           
000400     03 OHASH-SORT-IDDISTR   PIC 9(4).                                    
000500*                                 DISTRIKTNUMMER                          
000600     03 OHASH-SORT-TIFILDAT  PIC 9(6).                                    
000700*                                 DATUM NÄR EN FIL SKAPATS ÅÅMMDD         
000800     03 OHASH-SORT-TIHHMMSS  PIC 9(6).                                    
000900*                                 TIM - MIN - SEK   (HHMMSS)              
001000     03 OHASH-IDPTYP         PIC X(3).                                    
001100*                                 POSTTYP                                 
001200     03 OHASH-IDDISTR        PIC 9(4).                                    
001300*                                 DISTRIKTNUMMER                          
001400     03 OHASH-IDKUNDNR       PIC 9(6).                                    
001500*                                 KUNDNUMMER                              
001600     03 OHASH-IDORDNR        PIC 9(7).                                    
001700*                                 ORDERNR             IDORDNR-002         
001800     03 OHASH-SUHASH         PIC 9(14).                                   
001900*                                 HASH-TOTAL  = SUMMA AV ART.NR.          
002000*                                  KONTROLLSIFFRA OCH BESTÄLLT            
002100*                                  ANTAL FÖR ALLA RADER INOM              
002200*                                  ORDERN                                 
002300     03 OHASH-IDTRANSLOP     PIC 9(5).                                    
002400*                                 TRANSAKTIONS-LÖPNUMMER                  
002500     03 OHASH-KDFEL          PIC 9(3).                                    
002600*                                 FELKOD                                  
002700     03 OHASH-SUHASH-RAETT   PIC 9(14).                                   
002800*                                 HASH-TOTAL  = SUMMA AV ART.NR.          
002900*                                  KONTROLLSIFFRA OCH BESTÄLLT            
003000*                                  ANTAL FÖR ALLA RADER INOM              
003100*                                  ORDERN                                 
003200*** END COPY W460003CC0  LENGTH=72                                        
