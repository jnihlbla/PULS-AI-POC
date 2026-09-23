000100 01  MOD-W5O12201.                                                        
000200*                                 MOD-COPYTEXT FÖR BILD 5122              
000300*                                 EXISTING PRICES, SUBVENDORS             
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDARTNR-IN       PIC Z(8)9.                                   
000900*                                 ARTIKELNUMMER                           
001000     03 MOD-IDARTNR-UT       PIC Z(8)9.                                   
001100*                                 ARTIKELNUMMER                           
001200     03 MOD-TABELLRAD        OCCURS 14 TIMES.                             
001300*                                 GRUPP MED TABELL RADER                  
001400        05 MOD-KDPRURSP      PIC X.                                       
001500*                                 PRISHÄRSTAMNING BESTÄLLNING             
001600        05 MOD-DAPRLIST      PIC 9(8).                                    
001700*                                 PRISLISTEDATUM (AAAAMMDD)               
001800        05 MOD-IDLEVNR       PIC X(5).                                    
001900*                                 LEVERANTÖRNUMMER                        
002000        05 MOD-PRARTBEL-PR   PIC Z(7)9.9(5).                              
002100*                                 DETTA BESTÄLLNINGSPRIS                  
002200*                                 (I LEVERANTÖRENS VALUTA)                
002300        05 MOD-KDVALISO      PIC X(3).                                    
002400*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
002500        05 MOD-FLHUVLEV      PIC X(3).                                    
002600        05 MOD-KDSTATUS      PIC X(5).                                    
002700        05 MOD-KDFPKPRI      PIC X.                                       
002800*                                 OM FÖRPACKNING INGÅR I ARTPRIS          
002900     03 MOD-TEMFSINF         PIC X(55).                                   
003000*                                 INFORMATIONSMEDDELANDE                  
003100*** END OF VILMAII-COPY LENGTH= 677 BYTES                                 
