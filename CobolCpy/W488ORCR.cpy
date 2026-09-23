000100 01  SYNQ-W488ORCR.                                                       
000200*                                 COPYBOOK FOR ORDER CREATION             
000300*                                 SUB PROGRAM WHICH IS CALLED             
000400*                                 FROM 6164                               
000500     03 SYNQ-ORDERTYPE       PIC X(3).                                    
000600     03 SYNQ-IDDC            PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800     03 SYNQ-IDARTNR         PIC 9(9).                                    
000900*                                 ARTIKELNUMMER                           
001000     03 SYNQ-TIORDTIME       PIC 9(12).                                   
001100     03 SYNQ-KVBEST          PIC 9(6).                                    
001200*                                 BESTÄLLT ANTAL                          
001300     03 SYNQ-ADLAGOMR-TOM    PIC 9(2).                                    
001400*                                 LAGEROMRÅDE TILL OCH MED                
001500     03 SYNQ-ADGANG-TOM      PIC 9(2).                                    
001600*                                 GÅNG TILL OCH MED                       
001700     03 SYNQ-ADPLATS-TOM     PIC 9(5).                                    
001800*                                 LAGERPLATS TILL OCH MED                 
001900     03 SYNQ-TREATED         PIC X.                                       
002000     03 SYNQ-LOCATION        PIC X(10).                                   
002100     03 SYNQ-TIRFSDAT        PIC 9(6).                                    
002200*                                 KLART FÖR TRANSPORT ÅÅMMDD              
002300     03 SYNQ-IDPRODNR        PIC 9(7).                                    
002400*                                 PRODUKTIONSNUMMER                       
002500     03 SYNQ-IDRADNR         PIC 9(4).                                    
002600*                                 RADNUMMER                               
002700     03 SYNQ-KDORDKL         PIC 9.                                       
002800*                                 ORDERKLASS                              
002900     03 SYNQ-FLSATS          PIC X.                                       
003000*                                 SATSARTIKEL                             
003100     03 SYNQ-IDBORD          PIC X(3).                                    
003200*                                 PACK-BORD                               
003300     03 SYNQ-IDKUNDRF        PIC X(10).                                   
003400*                                 KUNDENS REFERENS (ORDERID)              
003500     03 SYNQ-KDSVAR          PIC X.                                       
003600*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
003700     03 SYNQ-IDPLKLST        PIC 9(3).                                    
003800*                                 PLOCKLISTNUMMER                         
003900     03 SYNQ-IDPRC.                                                       
004000*                                 PRODUKTIONSKANAL                        
004100        05 SYNQ-IDPRCBAS     PIC X(3).                                    
004200*                                 PRC-BAS                                 
004300        05 SYNQ-IDPRCVAR     PIC X.                                       
004400*                                 PRC-VARIANT                             
004500*** END OF VILMAII-COPY LENGTH= 92 BYTES                                  
