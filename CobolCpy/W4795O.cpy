000100 01  W4795O.                                                              
000200*                                 DAGLIG OKS- OCH EFR-INFO                
000300     03 IDDISTR              PIC 9(4).                                    
000400*                                 DISTRIKTNUMMER                          
000500     03 IDKUNDNR             PIC 9(6).                                    
000600*                                 KUNDNUMMER                              
000700     03 IDKUNDRF             PIC X(10).                                   
000800*                                 KUNDENS REFERENS (ORDERID)              
000900     03 IDDC                 PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 IDPRC.                                                            
001200*                                 PRODUKTIONSKANAL                        
001300        05 IDPRCBAS          PIC X(3).                                    
001400*                                 PRC-BAS                                 
001500        05 IDPRCVAR          PIC X.                                       
001600*                                 PRC-VARIANT                             
001700     03 KDORDKL              PIC 9.                                       
001800*                                 ORDERKLASS                              
001900     03 KVBEART-Q            PIC 9(6).                                    
002000*                                 BESTÄLLT KVANTANPASSAT ANTAL            
002100     03 KVEFRS-OPACK         PIC 9(7).                                    
002200*                                 EJ FAKTURERAT OPACKAT ANTAL ST          
002300     03 KVEFRS-PACK          PIC 9(7).                                    
002400*                                 PACKAT EJ FAKTURERAT ANTAL STYC         
002500*                                 K                                       
002600     03 PRARTSTD             PIC 9(7)V9(2).                               
002700*                                 ARTIKELSTANDARDPRIS                     
002800     03 PRAVCOST             PIC 9(7)V9(2).                               
002900*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
003000     03 VKARTNTO             PIC 9(4)V9(3).                               
003100*                                 ARTIKELVIKT NETTO (KG)                  
003200     03 VLARTNTO             PIC 9(8)V9(1).                               
003300*                                 ARTIKELVOLYM NETTO (CM3)                
003400*** END OF VILMAII-COPY LENGTH= 81 BYTES                                  
