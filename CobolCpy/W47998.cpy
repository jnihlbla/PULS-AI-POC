000100 01  W47998.                                                              
000200*                                 Q4-INFO KOMPLETTERAD M. Q2-INFO         
000300     03 IDORDER              PIC S9(7)           COMP-3.                  
000400*                                 VOLVO PARTS ORDERNUMMER                 
000500     03 IDDC                 PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 IDPRC.                                                            
000800*                                 PRODUKTIONSKANAL                        
000900        05 IDPRCBAS          PIC X(3).                                    
001000*                                 PRC-BAS                                 
001100        05 IDPRCVAR          PIC X.                                       
001200*                                 PRC-VARIANT                             
001300     03 KDODELSTA            PIC X.                                       
001400*                                 ORDERDELSTATUS                          
001500     03 IDDISTR              PIC S9(5)           COMP-3.                  
001600*                                 DISTRIKTNUMMER                          
001700     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
001800*                                 KUNDNUMMER                              
001900     03 IDKUNDRF             PIC X(10).                                   
002000*                                 KUNDENS REFERENS (ORDERID)              
002100     03 IDARTNR              PIC S9(9)           COMP-3.                  
002200*                                 ARTIKELNUMMER                           
002300     03 KVBEART-Q            PIC S9(7)           COMP-3.                  
002400*                                 BESTÄLLT KVANTANPASSAT ANTAL            
002500     03 KDFRAKT              PIC S9(3)           COMP-3.                  
002600*                                 FRAKTSÄTT DC TILL KUND                  
002700     03 KDORDKL              PIC S9              COMP-3.                  
002800*                                 ORDERKLASS                              
002900     03 TIREGDAT             PIC S9(7)           COMP-3.                  
003000*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
003100     03 ADLAGOMR             PIC S9(3)           COMP-3.                  
003200*                                 LAGEROMRÅDE                             
003300     03 IDLEVNR              PIC X(5).                                    
003400*                                 LEVERANTÖRNUMMER                        
003500     03 VKART                PIC S9(7)           COMP-3.                  
003600*                                 ARTIKELVIKT (G)                         
003700     03 VLARTNTO             PIC S9(8)V9(1)      COMP-3.                  
003800*                                 ARTIKELVOLYM NETTO (CM3)                
003900     03 PRARTNTO             PIC S9(7)V9(2)      COMP-3.                  
004000*                                 ARTIKELPRIS NETTO                       
004100     03 KDPRODSL             PIC S9(3)           COMP-3.                  
004200*                                 PRODUKTSLAG                             
004300*** END OF VILMAII-COPY LENGTH= 67 BYTES                                  
