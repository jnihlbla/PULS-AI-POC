000100 01  W412RX3.                                                             
000200*                                 TYP = RX3, LÄNGD = 80                   
000300*                                                                         
000400     03 IDTYP                PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 IDDISTR              PIC 9(4).                                    
000700*                                 DISTRIKTNUMMER                          
000800     03 IDKUNDNR             PIC 9(6).                                    
000900*                                 KUNDNUMMER                              
001000     03 IDORDNR7             PIC 9(7).                                    
001100*                                 ORDERNUMMER                             
001200     03 KDCLAGER             PIC 9.                                       
001300*                                 CENTRALLAGERKOD                         
001400     03 KDORDKL              PIC 9.                                       
001500*                                 ORDERKLASS                              
001600     03 KDFAKTYP             PIC X.                                       
001700*                                 FAKTURATYP                              
001800     03 KDFRAKT              PIC 9(2).                                    
001900*                                 FRAKTSÄTT DC TILL KUND                  
002000     03 KDTPOTYP             PIC 9.                                       
002100*                                 TYP AV TIDPLANERAD ORDER                
002200     03 BEKUNDRF             PIC X(10).                                   
002300*                                 KUNDENS REFERENS                        
002400     03 TITPO                PIC 9(6).                                    
002500*                                 PLANERAD ORDERDATUM                     
002600     03 IDARTNR              PIC 9(9).                                    
002700*                                 ARTIKELNUMMER                           
002800     03 REKSIFFR             PIC 9.                                       
002900*                                 KONTROLLSIFFRA                          
003000     03 KVBEART              PIC 9(6).                                    
003100*                                 BESTÄLLT ANTAL STYCKEN                  
003200     03 KDKVBRYT             PIC 9.                                       
003300*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
003400     03 BERADREF             PIC X(10).                                   
003500*                                 KUNDENS RADREFERENS                     
003600     03 ADLAGOMR-CD          PIC 9(2).                                    
003700*                                 LAGEROMRÅDE CROSS DOCKING LAGER         
003800     03 ADGANG-CD            PIC 9(2).                                    
003900*                                 GÅNG                                    
004000     03 ADPLATS-CD           PIC 9(5).                                    
004100*                                 LAGERPLATSNUMMER                        
004200     03 FILLER               PIC X(2).                                    
004300*** END OF VILMAII-COPY LENGTH= 80 BYTES                                  
