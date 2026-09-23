000100 01  OHASH-W460009.                                                       
000200*                                 ORDER HASH-TOTAL                        
000300*                                 POSTTYP = RHC  NOAC-DO                  
000400     03 OHASH-SORT-IDDISTR   PIC 9(4).                                    
000500*                                 DISTRIKTNUMMER                          
000600     03 OHASH-SORT-TIFILDAT  PIC 9(6).                                    
000700*                                 DATUM NÄR EN FIL SKAPATS ÅÅMMDD         
000800     03 OHASH-SORT-TIHHMMSS  PIC 9(6).                                    
000900*                                 TIM - MIN - SEK   (HHMMSS)              
001000     03 OHASH-SORT-IDLOPNR-FIL                                            
001100                             PIC 9(5).                                    
001200*                                 TRANSAKTIONS-LÖPNUMMER                  
001300     03 OHASH-SORT-IDLOPNR   PIC 9(5).                                    
001400*                                 TRANSAKTIONS-LÖPNUMMER                  
001500     03 OHASH-IDPTYP         PIC X(3).                                    
001600*                                 POSTTYP                                 
001700     03 OHASH-IDDISTR        PIC 9(4).                                    
001800*                                 DISTRIKTNUMMER                          
001900     03 OHASH-IDKUNDNR       PIC 9(6).                                    
002000*                                 KUNDNUMMER                              
002100     03 OHASH-IDORDNR        PIC 9(7).                                    
002200*                                 ORDERNR             IDORDNR-002         
002300     03 OHASH-SUHASH         PIC 9(14).                                   
002400*                                 HASH-TOTAL  = SUMMA AV ART.NR.          
002500*                                  KONTROLLSIFFRA OCH BESTÄLLT            
002600*                                  ANTAL FÖR ALLA RADER INOM              
002700*                                  ORDERN                                 
002800     03 FILLER               PIC X(3).                                    
002900     03 OHASH-SUHASH-RAETT   PIC 9(14).                                   
003000*                                 HASH-TOTAL  = SUMMA AV ART.NR.          
003100*                                  KONTROLLSIFFRA OCH BESTÄLLT            
003200*                                  ANTAL FÖR ALLA RADER INOM              
003300*                                  ORDERN                                 
003400     03 OHASH-KDFEL          PIC 9(3).                                    
003500*                                 FELKOD                                  
003600*** END COPY W460009CC0  LENGTH=80                                        
