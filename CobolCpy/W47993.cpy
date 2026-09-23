000100 01  W47993.                                                              
000200*                                 DAGLIG OKS- OCH EFR-INFO                
000300     03 IDDISTR              PIC 9(4).                                    
000400*                                 DISTRIKTNUMMER                          
000500     03 IDKUNDNR             PIC 9(6).                                    
000600*                                 KUNDNUMMER                              
000700     03 IDKUNDRF             PIC X(10).                                   
000800*                                 KUNDENS REFERENS (ORDERID)              
000900     03 IDPRODNR             PIC 9(7).                                    
001000*                                 PRODUKTIONSNUMMER                       
001100     03 IDPLKLST             PIC 9(3).                                    
001200*                                 PLOCKLISTNUMMER                         
001300     03 IDARTNR              PIC 9(8).                                    
001400*                                 ARTIKELNUMMER                           
001500     03 IDDC                 PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700     03 KVBEART-Q            PIC 9(6).                                    
001800*                                 BESTÄLLT KVANTANPASSAT ANTAL            
001900     03 KVEFRS-PACK          PIC 9(7).                                    
002000*                                 PACKAT EJ FAKTURERAT ANTAL STYC         
002100*                                 K                                       
002200     03 KVEFRS-OPACK         PIC 9(7).                                    
002300*                                 EJ FAKTURERAT OPACKAT ANTAL ST          
002400     03 KDFRAKT              PIC 9(2).                                    
002500*                                 FRAKTSÄTT DC TILL KUND                  
002600     03 KDORDKL              PIC 9.                                       
002700*                                 ORDERKLASS                              
002800     03 TIBEGPAC             PIC 9(6).                                    
002900*                                 BEGÄRD PACKNINGSDAG    (ÅÅMMDD)         
003000     03 TIORDREG             PIC 9(6).                                    
003100*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
003200     03 ADLAGOMR             PIC 9(2).                                    
003300*                                 LAGEROMRÅDE                             
003400     03 VLARTNTO             PIC 9(8)V9(1).                               
003500*                                 ARTIKELVOLYM NETTO (CM3)                
003600     03 VKARTNTO             PIC 9(4)V9(3).                               
003700*                                 ARTIKELVIKT NETTO (KG)                  
003800     03 PRARTSTD             PIC 9(7)V9(2).                               
003900*                                 ARTIKELSTANDARDPRIS                     
004000     03 PRARTNTO             PIC 9(7)V9(2).                               
004100*                                 ARTIKELPRIS NETTO                       
004200     03 IDPRC.                                                            
004300*                                 PRODUKTIONSKANAL                        
004400        05 IDPRCBAS          PIC X(3).                                    
004500*                                 PRC-BAS                                 
004600        05 IDPRCVAR          PIC X.                                       
004700*                                 PRC-VARIANT                             
004800     03 KDPRODSL             PIC 9(2).                                    
004900*                                 PRODUKTSLAG                             
005000     03 FLDIRLEV             PIC X.                                       
005100*                                 DIREKTLEVERANS ?                        
005200*** END OF VILMAII-COPY LENGTH= 118 BYTES                                 
