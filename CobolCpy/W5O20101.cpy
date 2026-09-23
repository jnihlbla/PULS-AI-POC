000100 01  MOD-W5O20101.                                                        
000200*                                 MOD-COPYTEXT FÖR W5020100               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-IDDC-IN          PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 MOD-IDARTNR-UT       PIC X(9).                                    
001200*                                 ARTIKELNUMMER                           
001300     03 MOD-IDDC-UT          PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 MOD-BEART            PIC X(25).                                   
001600*                                 ARTIKELBENÄMNING                        
001700     03 MOD-KDPSLLOC         PIC 9(2).                                    
001800*                                 PRODUKTSLAG LOKALT                      
001900     03 MOD-KDPRODSL         PIC Z9.                                      
002000*                                 PRODUKTSLAG                             
002100     03 MOD-KVAKS-PAV        PIC -(7)9.                                   
002200*                                 DEL AV AK PÅ VÄG                        
002300     03 MOD-KVAKS-SDC        PIC -(7)9.                                   
002400*                                 DEL AV AK SOM LIGGER I SDC              
002500     03 MOD-KVLS             PIC -(7)9.                                   
002600*                                 LAGERSALDO                              
002700     03 MOD-KVEFRS           PIC -(7)9.                                   
002800*                                 EJ FAKTURERAT ANTAL STYCK               
002900     03 MOD-SULAGVDE         PIC Z(8)9.9(2).                              
003000*                                 LAGERVÄRDE PER ARTIKEL                  
003100     03 MOD-PRAVCOST         PIC Z(6)9.9(2).                              
003200*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
003300     03 MOD-TIAVCOST         PIC 9(6).                                    
003400*                                 DATUM DÅ MEDELVÄRDET BERÄKNADES         
003500     03 MOD-PRMATRL          PIC Z(6)9.9(2).                              
003600*                                 FAST PRIS UNDER LÖPANDE ÅR              
003700     03 MOD-PRAVCOST-IN-ATTR PIC X(2).                                    
003800*                                 MFS ATTRIBUTFÄLT                        
003900     03 MOD-PRAVCOST-IN      PIC Z(6)9.9(2).                              
004000*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
004100     03 MOD-SULAGVDE-IN-ATTR PIC X(2).                                    
004200*                                 MFS ATTRIBUTFÄLT                        
004300     03 MOD-SULAGVDE-IN      PIC Z(8)9.9(2).                              
004400*                                 LAGERVÄRDE PER ARTIKEL                  
004500     03 MOD-KDAVCOST-IN-ATTR PIC X(2).                                    
004600*                                 MFS ATTRIBUTFÄLT                        
004700     03 MOD-KDAVCOST-IN      PIC X(2).                                    
004800*                                 ORSAKSKOD MANUELL KOST ÄNDRING          
004900     03 MOD-IDFS-IN-ATTR     PIC X(2).                                    
005000*                                 MFS ATTRIBUTFÄLT                        
005100     03 MOD-IDFS-IN          PIC X(8).                                    
005200*                                 FÖLJESEDELSNUMMER ENL ODETTE            
005300     03 MOD-PRMATRL-IN-ATTR  PIC X(2).                                    
005400*                                 MFS ATTRIBUTFÄLT                        
005500     03 MOD-PRMATRL-IN       PIC Z(6)9.9(2).                              
005600*                                 FAST PRIS UNDER LÖPANDE ÅR              
005700     03 MOD-TEMFSINF         PIC X(55).                                   
005800*                                 INFORMATIONSMEDDELANDE                  
005900*** END OF VILMAII-COPY LENGTH= 272 BYTES                                 
