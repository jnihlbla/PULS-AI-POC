000100 01  MOD-W5O11501-CTX.                                                    
000200*                                 MOD-COPYTEXT FÖR W5011500               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-IDARTNR-UT       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-IDLEVNR-IN       PIC X(5).                                    
001200*                                 LEVERANTÖRNUMMER                        
001300     03 MOD-IDLEVNR-UT       PIC X(5).                                    
001400*                                 LEVERANTÖRNUMMER                        
001500     03 MOD-KDPRBEH-IN       PIC X.                                       
001600*                                 PRIS BEHANDLAD ARTIKEL                  
001700     03 MOD-KDPRBEH-UT       PIC X.                                       
001800*                                 PRIS BEHANDLAD ARTIKEL                  
001900     03 MOD-REAENDR-IN       PIC Z(3)9.9.                                 
002000*                                 ÄNDRINGSPROCENT                         
002100     03 MOD-REAENDR-UT       PIC Z(3)9.9.                                 
002200*                                 ÄNDRINGSPROCENT                         
002300     03 MOD-IDLEVNR-ENTER    PIC X(5).                                    
002400*                                 LEVERANTÖRNUMMER                        
002500     03 MOD-IDLEVNR-NEXT     PIC X(5).                                    
002600*                                 LEVERANTÖRNUMMER                        
002700     03 MOD-IDARTNR-ENTER    PIC X(9).                                    
002800*                                 ARTIKELNUMMER                           
002900     03 MOD-IDARTNR-NEXT     PIC X(9).                                    
003000*                                 ARTIKELNUMMER                           
003100     03 MOD-KDPRBEH-ENTER    PIC X.                                       
003200*                                 PRIS BEHANDLAD ARTIKEL                  
003300     03 MOD-KDPRBEH-NEXT     PIC X.                                       
003400*                                 PRIS BEHANDLAD ARTIKEL                  
003500     03 MOD-REAENDR-ENTER    PIC Z(3)9.9.                                 
003600*                                 ÄNDRINGSPROCENT                         
003700     03 MOD-REAENDR-NEXT     PIC Z(3)9.9.                                 
003800*                                 ÄNDRINGSPROCENT                         
003900     03 MOD-TIUPPDAT         PIC 9(6).                                    
004000*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
004100     03 MOD-W5O11501-001-GRP OCCURS 12 TIMES.                             
004200        05 MOD-SELECT-URVAL-ATTR                                          
004300                             PIC X(2).                                    
004400*                                 MFS ATTRIBUTFÄLT                        
004500        05 MOD-SELECT-URVAL  PIC X.                                       
004600        05 MOD-IDARTNR       PIC Z(8)9.                                   
004700*                                 ARTIKELNUMMER                           
004800        05 MOD-PRARTBEL-PR   PIC Z(7)9.9(5).                              
004900*                                 DETTA BESTÄLLNINGSPRIS                  
005000*                                 (I LEVERANTÖRENS VALUTA)                
005100        05 MOD-KDVALISO      PIC X(3).                                    
005200*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
005300        05 MOD-PRINK-AKT     PIC Z(6)9.9(2).                              
005400*                                 INKÖPSPRIS AKTUELLT ÅR                  
005500        05 MOD-PRINK-KOM     PIC Z(6)9.9(2).                              
005600*                                 INKÖPSPRIS NÄSTA ÅR                     
005700        05 MOD-SPIS-PRARTSTD PIC Z(6)9.9(2).                              
005800*                                 ARTIKELSTANDARDPRIS                     
005900        05 MOD-REAENDR-INK   PIC Z(3)9.9-.                                
006000*                                 ÄNDRINGSPROCENT                         
006100        05 MOD-KDPRBEH-IN-UT-ATTR                                         
006200                             PIC X(2).                                    
006300*                                 MFS ATTRIBUTFÄLT                        
006400        05 MOD-KDPRBEH-IN-UT PIC X.                                       
006500*                                 PRIS BEHANDLAD ARTIKEL                  
006600     03 MOD-TEMFSINF         PIC X(55).                                   
006700*                                 INFORMATIONSMEDDELANDE                  
006800*** END OF VILMAII-COPY LENGTH= 1017 BYTES                                
