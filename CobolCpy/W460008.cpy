000100 01  ORAD-W460008.                                                        
000200*                                 ORDERRAD-TRANSAKTIONER                  
000300*                                 POSTTYP = RHB  NOAC-DO                  
000400     03 ORAD-SORT-IDDISTR    PIC 9(4).                                    
000500*                                 DISTRIKTNUMMER                          
000600     03 ORAD-SORT-TIFILDAT   PIC 9(6).                                    
000700*                                 DATUM NÄR EN FIL SKAPATS ÅÅMMDD         
000800     03 ORAD-SORT-TIHHMMSS   PIC 9(6).                                    
000900*                                 TIM - MIN - SEK   (HHMMSS)              
001000     03 ORAD-SORT-IDLOPNR-FIL                                             
001100                             PIC 9(5).                                    
001200*                                 TRANSAKTIONS-LÖPNUMMER                  
001300     03 ORAD-SORT-IDLOPNR    PIC 9(5).                                    
001400*                                 TRANSAKTIONS-LÖPNUMMER                  
001500     03 ORAD-IDPTYP          PIC X(3).                                    
001600*                                 POSTTYP                                 
001700     03 ORAD-IDDISTR         PIC 9(4).                                    
001800*                                 DISTRIKTNUMMER                          
001900     03 ORAD-IDKUNDNR        PIC 9(6).                                    
002000*                                 KUNDNUMMER                              
002100     03 ORAD-IDORDNR         PIC 9(7).                                    
002200*                                 ORDERNR             IDORDNR-002         
002300     03 ORAD-IDARTNR         PIC 9(9).                                    
002400*                                 ARTIKELNUMMER                           
002500     03 ORAD-REKSIFFR        PIC 9.                                       
002600*                                 KONTROLLSIFFRA                          
002700     03 ORAD-BERADREF        PIC X(10).                                   
002800*                                 KUNDENS RADREFERENS                     
002900     03 ORAD-KVBEART         PIC 9(6).                                    
003000*                                 BESTÄLLT ANTAL ARTIKLAR                 
003100     03 ORAD-KDKVBRYT        PIC 9.                                       
003200*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
003300     03 ORAD-KDDSP           PIC 9.                                       
003400*                                 PÅVERKAN PÅ DSP                         
003500     03 FILLER               PIC X(2).                                    
003600     03 ORAD-REKSIFFR-RAETT  PIC 9.                                       
003700*                                 KONTROLLSIFFRA                          
003800     03 ORAD-KDFEL           PIC 9(3).                                    
003900*                                 FELKOD                                  
004000*** END COPY W460008CC0  LENGTH=80                                        
