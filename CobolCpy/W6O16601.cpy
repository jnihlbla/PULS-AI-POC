000100 01  MOD-W6O16601.                                                        
000200*                                 MOD COPYTEXT FÖR W6O16600               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-IDARTNR-UT       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-IDLAYOUT-ATTR    PIC X(2).                                    
001200*                                 MFS ATTRIBUTFÄLT                        
001300     03 MOD-IDLAYOUT-IN      PIC X(10).                                   
001400*                                 ETIKETTSLAYOUT ID                       
001500     03 MOD-IDARTNR-ETIK-ATTR                                             
001600                             PIC X(2).                                    
001700*                                 MFS ATTRIBUTFÄLT                        
001800     03 MOD-IDARTNR-ETIK-IN  PIC X(9).                                    
001900*                                 ARTNR FÖR LEVERANTÖRENS BEABS E         
002000*                                 TIKETTER                                
002100     03 MOD-IDLAYOUT-UT      PIC X(10).                                   
002200*                                 ETIKETTSLAYOUT ID                       
002300     03 MOD-IDARTNR-ETIK-UT  PIC Z(8)9.                                   
002400*                                 ARTNR FÖR LEVERANTÖRENS BEABS E         
002500*                                 TIKETTER                                
002600     03 MOD-TEETIK-INT-ATTR  PIC X(2).                                    
002700*                                 MFS ATTRIBUTFÄLT                        
002800     03 MOD-TEETIK-INT-IN    PIC X(60).                                   
002900*                                 ETIKETT KOMMENTAR                       
003000     03 MOD-TEETIK-INT-UT    PIC X(60).                                   
003100*                                 ETIKETT KOMMENTAR                       
003200     03 MOD-BEART            PIC X(25).                                   
003300*                                 ARTIKELBENÄMNING                        
003400     03 MOD-IDUSER           PIC X(8).                                    
003500*                                 ANVÄNDARENS SÄKERHETS ID                
003600     03 MOD-TIUPPDAT         PIC 9(6).                                    
003700*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
003800     03 MOD-DAREGDAT         PIC 9(8).                                    
003900*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
004000     03 MOD-TEMFSINF         PIC X(55).                                   
004100*                                 INFORMATIONSMEDDELANDE                  
004200*** END OF VILMAII-COPY LENGTH= 328 BYTES                                 
