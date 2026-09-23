000100 01  MID-W4I25301-CTX.                                                    
000200*                                 MID-COPYTEXT FÖR W4I25301               
000300*                                 ORDERBEKRÄFTELSER  FRÅN VIPS            
000400     03 MID-IDSYSTEM         PIC X(4).                                    
000500*                                 VOLVO VCAS SYSTEMNUMMER                 
000600     03 MID-IDDISTR          PIC X(4).                                    
000700*                                 DISTRIKTNUMMER                          
000800     03 MID-IDKUNDNR         PIC X(6).                                    
000900*                                 KUNDNUMMER                              
001000     03 MID-IDORDNR          PIC X(7).                                    
001100*                                 ORDERNUMMER                             
001200     03 MID-KDORDKL          PIC 9.                                       
001300*                                 ORDERKLASS                              
001400     03 MID-FLSLUT           PIC X.                                       
001500*                                 AVSLUTNINGSFLAGGA                       
001600     03 MID-W4I25301-001-GRP OCCURS 14 TIMES.                             
001700        05 MID-IDARTNR       PIC X(9).                                    
001800*                                 ARTIKELNUMMER                           
001900        05 MID-IDLOPNR       PIC X(3).                                    
002000*                                 LÖPNUMMER                               
002100        05 MID-IDSEKVNR      PIC X(3).                                    
002200*                                 GENERELLT SEKVENSNUMMER                 
002300        05 MID-IDDC          PIC X(2).                                    
002400*                                 IDENTIFIERARE LAGER                     
002500        05 MID-KDORDBEK      PIC X(2).                                    
002600*                                 ORDERBEKRÄFTELSEKOD                     
002700        05 MID-KVBEART       PIC X(6).                                    
002800*                                 BESTÄLLT ANTAL STYCKEN                  
002900        05 MID-BEART-USA     PIC X(25).                                   
003000*                                 AMERIKANSK ART.BENÄMNING                
003100*** END OF VILMAII-COPY LENGTH= 723 BYTES                                 
