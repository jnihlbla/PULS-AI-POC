000100 01  MOD-W5O12501.                                                        
000200*                                 MOD-COPYTEXT FÖR BILD 5125              
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
001600        05 MOD-IDDC          PIC X(2).                                    
001700*                                 IDENTIFIERARE LAGER                     
001800        05 MOD-DAPRLIST      PIC 9(8).                                    
001900*                                 PRISLISTEDATUM (AAAAMMDD)               
002000        05 MOD-IDLEVNR       PIC X(5).                                    
002100*                                 LEVERANTÖRNUMMER                        
002200        05 MOD-PRARTBEL-PR   PIC Z(7)9.9(5).                              
002300*                                 DETTA BESTÄLLNINGSPRIS                  
002400*                                 (I LEVERANTÖRENS VALUTA)                
002500        05 MOD-KDVALISO      PIC X(3).                                    
002600*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
002700        05 MOD-KDSTATUS      PIC X(5).                                    
002800        05 MOD-DAREGDAT      PIC 9(8).                                    
002900*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
003000        05 MOD-IDUSER        PIC X(8).                                    
003100*                                 ANVÄNDARENS SÄKERHETS ID                
003200     03 MOD-TEMFSINF         PIC X(55).                                   
003300*                                 INFORMATIONSMEDDELANDE                  
003400*** END OF VILMAII-COPY LENGTH= 873 BYTES                                 
