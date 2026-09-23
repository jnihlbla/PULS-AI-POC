000100 01  W47991.                                                              
000200*                                 CTXT FÖR SKAPANDE AV PREEXTRAKT         
000300*                                 FÖR WDQ2.                               
000400     03 IDDISTR              PIC S9(5)           COMP-3.                  
000500*                                 DISTRIKTNUMMER                          
000600     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
000700*                                 KUNDNUMMER                              
000800     03 IDKUNDRF             PIC X(10).                                   
000900*                                 KUNDENS REFERENS (ORDERID)              
001000     03 IDPRODNR             PIC S9(7)           COMP-3.                  
001100*                                 PRODUKTIONSNUMMER                       
001200     03 IDARTNR              PIC S9(9)           COMP-3.                  
001300*                                 ARTIKELNUMMER                           
001400     03 IDDC                 PIC X(2).                                    
001500*                                 IDENTIFIERARE LAGER                     
001600     03 KVBEART-Q            PIC S9(7)           COMP-3.                  
001700*                                 BESTÄLLT KVANTANPASSAT ANTAL            
001800     03 KDFRAKT              PIC S9(3)           COMP-3.                  
001900*                                 FRAKTSÄTT DC TILL KUND                  
002000     03 KDORDKL              PIC S9              COMP-3.                  
002100*                                 ORDERKLASS                              
002200     03 TIBEGPAC             PIC S9(7)           COMP-3.                  
002300*                                 BEGÄRD PACKNINGSDAG    (ÅÅMMDD)         
002400     03 TIREGDAT             PIC S9(7)           COMP-3.                  
002500*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
002600     03 ADLAGOMR             PIC S9(3)           COMP-3.                  
002700*                                 LAGEROMRÅDE                             
002800     03 VKART                PIC S9(7)           COMP-3.                  
002900*                                 ARTIKELVIKT (G)                         
003000     03 VLARTNTO             PIC S9(8)V9(1)      COMP-3.                  
003100*                                 ARTIKELVOLYM NETTO (CM3)                
003200     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
003300*                                 ARTIKELSTANDARDPRIS                     
003400     03 PRARTNTO             PIC S9(7)V9(2)      COMP-3.                  
003500*                                 ARTIKELPRIS NETTO                       
003600     03 IDPRC.                                                            
003700*                                 PRODUKTIONSKANAL                        
003800        05 IDPRCBAS          PIC X(3).                                    
003900*                                 PRC-BAS                                 
004000        05 IDPRCVAR          PIC X.                                       
004100*                                 PRC-VARIANT                             
004200     03 KDPRODSL             PIC S9(3)           COMP-3.                  
004300*                                 PRODUKTSLAG                             
004400     03 IDLEVNR              PIC X(5).                                    
004500*                                 LEVERANTÖRNUMMER                        
004600*** END OF VILMAII-COPY LENGTH= 75 BYTES                                  
