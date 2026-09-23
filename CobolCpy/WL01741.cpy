000100 01  DOC1-WL01741.                                                        
000200*                                 COPYTEXT FOR INVENTORY REQUEST          
000300*                                 2:1 LDC                                 
000400     03 DOC1-IDAFPRCD        PIC X(10).                                   
000500*                                 AFP-BLANKETT POSTTYP                    
000600     03 DOC1-IDDC            PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800     03 DOC1-TIUTSKR         PIC 9(6).                                    
000900*                                 UTSKRIFTDATUM  (ÅÅMMDD)                 
001000     03 DOC1-TIUTSTID        PIC 9(6).                                    
001100*                                 UTSKRIFTSTID (TTMMSS)                   
001200     03 DOC1-IDPRINTINV      PIC X(6).                                    
001300     03 DOC1-IDARTNR         PIC Z(7)9.                                   
001400*                                 ARTIKELNUMMER                           
001500     03 DOC1-BEART           PIC X(25).                                   
001600*                                 ARTIKELBENÄMNING                        
001700     03 DOC1-ADLAGOMR        PIC 9(2).                                    
001800*                                 LAGEROMRÅDE                             
001900     03 DOC1-ADGANG          PIC 9(2).                                    
002000*                                 GÅNG                                    
002100     03 DOC1-ADPLATS         PIC 9(5).                                    
002200*                                 LAGERPLATSNUMMER                        
002300     03 DOC1-BUFF-ADLAGOMR   PIC 9(2).                                    
002400*                                 LAGEROMRÅDE                             
002500     03 DOC1-BUFF-ADGANG     PIC 9(2).                                    
002600*                                 GÅNG                                    
002700     03 DOC1-BUFF-ADPLATS    PIC 9(5).                                    
002800*                                 LAGERPLATSNUMMER                        
002900     03 DOC1-BUFF-KVLS       PIC -(7)9.                                   
003000*                                 LAGERSALDO                              
003100     03 DOC1-KVLS            PIC -(7)9.                                   
003200*                                 LAGERSALDO                              
003300     03 DOC1-KVAKS           PIC -(7)9.                                   
003400*                                 ANKOMSTSALDO                            
003500     03 DOC1-KVEFRS          PIC -(7)9.                                   
003600*                                 EJ FAKTURERAT ANTAL STYCK               
003700*** END OF VILMAII-COPY LENGTH= 113 BYTES                                 
