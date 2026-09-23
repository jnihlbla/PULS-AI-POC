000100 01  WXTRA3.                                                              
000200*                                 DAGLIG OKS- OCH EFR-INFO                
000300     03 IDDISTR              PIC 9(4).                                    
000400*                                 DISTRIKTNUMMER                          
000500     03 IDKUNDNR             PIC 9(6).                                    
000600*                                 KUNDNUMMER                              
000700     03 IDKUNDRF             PIC X(10).                                   
000800*                                 KUNDENS REFERENS (ORDERID)              
000900     03 IDPRODNR             PIC 9(7).                                    
001000*                                 PRODUKTIONSNUMMER                       
001100     03 IDARTNR              PIC 9(8).                                    
001200*                                 ARTIKELNUMMER                           
001300     03 IDDC                 PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 KVBEART-Q            PIC 9(6).                                    
001600*                                 BESTÄLLT KVANTANPASSAT ANTAL            
001700     03 KVEFRS-PACK          PIC 9(7).                                    
001800*                                 PACKAT EJ FAKTURERAT ANTAL STYC         
001900*                                 K                                       
002000     03 KVEFRS-OPACK         PIC 9(7).                                    
002100*                                 EJ FAKTURERAT OPACKAT ANTAL ST          
002200     03 KVEFRS-SKEPP         PIC 9(7).                                    
002300*                                 EJ FAKTURERAT ANTAL STYCK               
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
004200     03 PRARTNTO-LOC         PIC 9(7)V9(2).                               
004300*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
004400     03 PRARTNTO-LOCPREL     PIC 9(7)V9(2).                               
004500*                                 PREL NETTO SLUTKUNDSPRIS I              
004600*                                 LOKAL VALUTA                            
004700     03 KDVALISO             PIC X(3).                                    
004800*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
004900     03 IDPRC.                                                            
005000*                                 PRODUKTIONSKANAL                        
005100        05 IDPRCBAS          PIC X(3).                                    
005200*                                 PRC-BAS                                 
005300        05 IDPRCVAR          PIC X.                                       
005400*                                 PRC-VARIANT                             
005500     03 KDPRODSL             PIC 9(2).                                    
005600*                                 PRODUKTSLAG                             
005700     03 FLDIRLEV             PIC X.                                       
005800*                                 DIREKTLEVERANS ?                        
005900*** END OF VILMAII-COPY LENGTH= 143 BYTES                                 
