000100 01  W47988.                                                              
000200*                                 EFR-INFO                                
000300     03 IDARTNR              PIC S9(9)           COMP-3.                  
000400*                                 ARTIKELNUMMER                           
000500     03 IDDISTR              PIC S9(5)           COMP-3.                  
000600*                                 DISTRIKTNUMMER                          
000700     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
000800*                                 KUNDNUMMER                              
000900     03 IDKUNDRF             PIC X(10).                                   
001000*                                 KUNDENS REFERENS (ORDERID)              
001100     03 IDPRODNR             PIC S9(7)           COMP-3.                  
001200*                                 PRODUKTIONSNUMMER                       
001300     03 IDDC                 PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 KVBEART-Q            PIC S9(7)           COMP-3.                  
001600*                                 BESTÄLLT KVANTANPASSAT ANTAL            
001700     03 KDFRAKT              PIC S9(3)           COMP-3.                  
001800*                                 FRAKTSÄTT DC TILL KUND                  
001900     03 KDORDKL              PIC S9              COMP-3.                  
002000*                                 ORDERKLASS                              
002100     03 TIBEGPAC             PIC S9(7)           COMP-3.                  
002200*                                 BEGÄRD PACKNINGSDAG    (ÅÅMMDD)         
002300     03 TIREGDAT             PIC S9(7)           COMP-3.                  
002400*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
002500     03 ADLAGOMR             PIC S9(3)           COMP-3.                  
002600*                                 LAGEROMRÅDE                             
002700     03 VKART                PIC S9(7)           COMP-3.                  
002800*                                 ARTIKELVIKT (G)                         
002900     03 VLARTNTO             PIC S9(8)V9(1)      COMP-3.                  
003000*                                 ARTIKELVOLYM NETTO (CM3)                
003100     03 PRARTNTO             PIC S9(7)V9(2)      COMP-3.                  
003200*                                 ARTIKELPRIS NETTO                       
003300     03 IDPRC.                                                            
003400*                                 PRODUKTIONSKANAL                        
003500        05 IDPRCBAS          PIC X(3).                                    
003600*                                 PRC-BAS                                 
003700        05 IDPRCVAR          PIC X.                                       
003800*                                 PRC-VARIANT                             
003900     03 KDPRODSL             PIC S9(3)           COMP-3.                  
004000*                                 PRODUKTSLAG                             
004100     03 IDLEVNR              PIC X(5).                                    
004200*                                 LEVERANTÖRNUMMER                        
004300*** END OF VILMAII-COPY LENGTH= 70 BYTES                                  
