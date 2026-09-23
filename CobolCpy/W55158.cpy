000100 01  W55158.                                                              
000200*                                  FIL MED UTDRAG UR WDC6-BASEN           
000300     03 IDLEVNR              PIC X(5).                                    
000400*                                 LEVERANTÖRNUMMER                        
000500     03 IDARTNR              PIC Z(7)9.                                   
000600*                                 ARTIKELNUMMER                           
000700     03 KDPRBEH              PIC X.                                       
000800*                                 PRIS BEHANDLAD ARTIKEL                  
000900     03 KDVALISO             PIC X(3).                                    
001000*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
001100     03 PRARTBEL-PR          PIC Z(7)9.9(5).                              
001200*                                 DETTA BESTÄLLNINGSPRIS                  
001300*                                 (I LEVERANTÖRENS VALUTA)                
001400     03 PRKURS               PIC Z(5)9.9(5).                              
001500*                                 VALUTAKURS                              
001600     03 RETULF-1             PIC Z(2)9.9(4).                              
001700*                                 TULLFAKTOR FRÅN OCH MED                 
001800*                                 TILLÄMPNINGSDATUM                       
001900     03 RETULF-2             PIC Z(2)9.9(4).                              
002000*                                 TULLFAKTOR FRAM TILL                    
002100*                                 TILLÄMPNINGSDATUM                       
002200     03 TITULF               PIC 9(6).                                    
002300*                                 TILLÄMPNINGSDATUM FÖR                   
002400*                                 TULLFAKTOR      (ÅÅMMDD)                
002500     03 TIPRLIST             PIC 9(6).                                    
002600*                                 PRISLISTEDATUM (AAMMDD)                 
002700     03 PRINK-KOM            PIC Z(6)9.9(2).                              
002800*                                 INKÖPSPRIS NÄSTA ÅR                     
002900     03 PRDIRLON-KOM         PIC Z(3)9.9(3).                              
003000*                                 DIREKT LÖN NÄSTA ÅR                     
003100     03 PRMATRL-KOM          PIC Z(6)9.9(2).                              
003200*                                 FAST PRIS UNDER LÖPANDE ÅR              
003300     03 PROVRPAL-KOM         PIC Z(3)9.9(3).                              
003400*                                 ÖVRIGA OMKOSTNADER PÅLÄGG               
003500*                                 NÄSTA ÅR                                
003600     03 REAENDR              PIC Z(3)9.9-.                                
003700*                                 ÄNDRINGSPROCENT                         
003800     03 IDPRANSV             PIC X(4).                                    
003900*                                 PRISANSVAR FÖR ARTIKELN                 
004000     03 FELTYP               PIC X(50).                                   
004100*                                                                         
004200*** END OF VILMAII-COPY LENGTH= 168 BYTES                                 
