000100 01  W27111.                                                              
000200*                                 REFILL                                  
000300*                                 COPYTEXT FÖR FILER SOM                  
000400*                                 UPPDATERAR AV WDE3 I PGM                
000500*                                 W27111                                  
000600*                                 REFILLORDRAR, REFILL-                   
000700*                                 FÖRSLAG, RETURER, LOKALA                
000800*                                 ARTIKLAR USA, FLYGFÖRSLAG               
000900*                                 USA, MANUELLA REFILLORDRAR              
001000*                                 OCH MANUELLA RETURER                    
001100     03 IDDC                 PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300     03 IDPERSON-BUY         PIC S9(3)           COMP-3.                  
001400*                                 PERSONKOD REFILLANSVARIG                
001500     03 KDREFTYP             PIC X.                                       
001600*                                 TYP AV REFILLORDER                      
001700     03 IDARTNR              PIC S9(9)           COMP-3.                  
001800*                                 ARTIKELNUMMER                           
001900     03 IDDISTR              PIC S9(5)           COMP-3.                  
002000*                                 DISTRIKTNUMMER                          
002100     03 ADLAGOMR-CDC         PIC S9(3)           COMP-3.                  
002200*                                 LAGEROMRÅDE                             
002300     03 ADGANG-CDC           PIC S9(3)           COMP-3.                  
002400*                                 GÅNG                                    
002500     03 ADPLATS-CDC          PIC S9(5)           COMP-3.                  
002600*                                 LAGERPLATSNUMMER                        
002700     03 ADLAGOMR-SDC         PIC S9(3)           COMP-3.                  
002800*                                 LAGEROMRÅDE                             
002900     03 ADGANG-SDC           PIC S9(3)           COMP-3.                  
003000*                                 GÅNG                                    
003100     03 ADPLATS-SDC          PIC S9(5)           COMP-3.                  
003200*                                 LAGERPLATSNUMMER                        
003300     03 KVBEART              PIC S9(7)           COMP-3.                  
003400*                                 BESTÄLLT ANTAL STYCKEN                  
003500     03 KDREFORS             PIC X.                                       
003600*                                 REFILL ORDER STATUSKOD                  
003700     03 IDLEVNR              PIC X(5).                                    
003800*                                 LEVERANTÖRNUMMER                        
003900     03 KDREFTXT             PIC 9(2).                                    
004000*                                 KOD FÖR REFILL VARNINGSTEXT             
004100     03 KDFRAKT              PIC S9(3)           COMP-3.                  
004200*                                 FRAKTSÄTT DC TILL KUND                  
004300     03 KVBEART-CD           PIC S9(7)           COMP-3.                  
004400*                                 BESTÄLLT ANTAL CROSS DOCKING            
004500     03 ADART-CD.                                                         
004600*                                 ARTIKELADRESS I CD-LAGRET               
004700        05 ADLAGOMR-CD       PIC S9(3)           COMP-3.                  
004800*                                 LAGEROMRÅDE                             
004900        05 ADGANG-CD         PIC S9(3)           COMP-3.                  
005000*                                 GÅNG                                    
005100        05 ADPLATS-CD        PIC S9(5)           COMP-3.                  
005200*                                 LAGERPLATSNUMMER                        
005300     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
005400*                                 KUNDNUMMER                              
005500     03 IDDC-REF             PIC X(2).                                    
005600*                                 SÄNDANDE LAGER FÖR REFILL               
005700*** END OF VILMAII-COPY LENGTH= 58 BYTES                                  
