000100 01  MOD-W1O52601.                                                        
000200*                                 MOD-COPYTEXT FÖR BILD 1526.             
000300*                                 BESTÄLLNING AV KATALOG-                 
000400*                                 BENÄMNING ÖVERSÄTTNING                  
000500     03 MOD-IDTRANS          PIC X(4).                                    
000600*                                 BILDNUMMER                              
000700     03 MOD-TEMFSFEL         PIC X(40).                                   
000800*                                 MFS FELMEDDELANDE                       
000900     03 MOD-IDAVD-IN-ATTR    PIC X(2).                                    
001000*                                 MFS ATTRIBUTFÄLT                        
001100     03 MOD-IDAVD-IN         PIC X(5).                                    
001200*                                 DEN ANSTÄLLDES AVDELNING/               
001300*                                 KOSTNADSSTÄLLE                          
001400     03 MOD-IDAVD-UT         PIC Z(4)9.                                   
001500*                                 DEN ANSTÄLLDES AVDELNING/               
001600*                                 KOSTNADSSTÄLLE                          
001700     03 MOD-IDBENNR-UPD-ATTR PIC X(2).                                    
001800*                                 MFS ATTRIBUTFÄLT                        
001900     03 MOD-IDBENNR-UPD      PIC X(7).                                    
002000*                                 BENÄMNINGSNUMMER                        
002100     03 MOD-IDORDER-BEN-ATTR PIC X(2).                                    
002200*                                 MFS ATTRIBUTFÄLT                        
002300     03 MOD-IDORDER-BEN      PIC 9(7).                                    
002400*                                 VOLVO PARTS ORDERNUMMER                 
002500     03 MOD-IDORDER-LEX-ATTR PIC X(2).                                    
002600*                                 MFS ATTRIBUTFÄLT                        
002700     03 MOD-IDORDER-LEX      PIC 9(7).                                    
002800*                                 VOLVO PARTS ORDERNUMMER                 
002900     03 MOD-TIUPPDAT-LEX     PIC 9(6).                                    
003000*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
003100     03 MOD-BENAMN-LISTA     OCCURS 28 TIMES.                             
003200        05 MOD-LIST-IDBENNR  PIC Z(7).                                    
003300*                                 BENÄMNINGSNUMMER                        
003400        05 MOD-LIST-BEART    PIC X(25).                                   
003500*                                 ARTIKELBENÄMNING                        
003600     03 MOD-TEMFSINF         PIC X(55).                                   
003700*                                 INFORMATIONSMEDDELANDE                  
003800*** END OF VILMAII-COPY LENGTH= 1040 BYTES                                
