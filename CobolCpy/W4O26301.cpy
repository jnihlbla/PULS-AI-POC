000100 01  MOD-W4O26301.                                                        
000200*                                 MOD AREA FÖR PROFORMARADER              
000300*                                                                         
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDDISTR-IN       PIC X(4).                                    
000900*                                 DISTRIKTNUMMER                          
001000     03 MOD-IDKUNDNR-IN      PIC X(6).                                    
001100*                                 KUNDNUMMER                              
001200     03 MOD-IDORDNR-IN       PIC X(7).                                    
001300*                                 ORDERNUMMER                             
001400     03 MOD-IDARTNR-IN       PIC X(9).                                    
001500*                                 ARTIKELNUMMER                           
001600     03 MOD-IDDISTR-UT       PIC X(4).                                    
001700*                                 DISTRIKTNUMMER                          
001800     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001900*                                 KUNDNUMMER                              
002000     03 MOD-IDORDNR-UT       PIC X(7).                                    
002100*                                 ORDERNUMMER                             
002200     03 MOD-KDORDKL-UT       PIC X.                                       
002300*                                 ORDERKLASS                              
002400     03 MOD-KDFRAKT-UT       PIC Z9.                                      
002500*                                 FRAKTSÄTT DC TILL KUND                  
002600     03 MOD-KDPROTYP-UT      PIC X.                                       
002700*                                 TYP AV PROFORMA                         
002800     03 MOD-KDORDBEK-NEXT    PIC 9(2).                                    
002900*                                 ORDERBEKRÄFTELSEKOD                     
003000     03 MOD-KDBEHX-NEXT      PIC X.                                       
003100*                                 BEHANDLINGSKOD-X                        
003200     03 MOD-IDARTNR-NEXT     PIC 9(9).                                    
003300*                                 ARTIKELNUMMER                           
003400     03 MOD-IDLOPNR-NEXT     PIC 9(3).                                    
003500*                                 LÖPNUMMER                               
003600     03 MOD-IDSEKVNR-NEXT    PIC 9(3).                                    
003700*                                 GENERELLT SEKVENSNUMMER                 
003800     03 MOD-TEDDI            PIC X(11).                                   
003900*                                 TEXTFÄLT DDI                            
004000     03 MOD-RAD              OCCURS 13 TIMES.                             
004100        05 MOD-KDORDBEK-ATTR PIC X(2).                                    
004200*                                 MFS ATTRIBUTFÄLT                        
004300        05 MOD-KDORDBEK      PIC 9(2).                                    
004400*                                 ORDERBEKRÄFTELSEKOD                     
004500        05 MOD-ASTERIX       PIC X.                                       
004600        05 MOD-KDBEHX-ATTR   PIC X(2).                                    
004700*                                 MFS ATTRIBUTFÄLT                        
004800        05 MOD-KDBEHX        PIC X.                                       
004900*                                 BEHANDLINGSKOD-X                        
005000        05 MOD-IDARTNR.                                                   
005100           07 MOD-IDARTNR-1--9                                            
005200                             PIC Z(8)9.                                   
005300*                                 ARTIKELNUMMER                           
005400           07 MOD-STRAEK     PIC X.                                       
005500           07 MOD-REKSIFFR   PIC 9.                                       
005600*                                 KONTROLLSIFFRA                          
005700        05 MOD-BEART         PIC X(15).                                   
005800*                                 ARTIKELBENÄMNING      BEART-002         
005900        05 MOD-KVBEART-ATTR  PIC X(2).                                    
006000*                                 MFS ATTRIBUTFÄLT                        
006100        05 MOD-KVBEART       PIC Z(7).                                    
006200*                                 BESTÄLLT ANTAL STYCKEN                  
006300        05 MOD-KVQPACK-ATTR  PIC X(2).                                    
006400*                                 MFS ATTRIBUTFÄLT                        
006500        05 MOD-KVQPACK       PIC Z(5).                                    
006600*                                 ANTAL KVANTITETFÖRPACKNINGAR            
006700        05 MOD-NYCKLAR       PIC X(16).                                   
006800     03 MOD-TEMFSINF         PIC X(55).                                   
006900*                                 INFORMATIONSMEDDELANDE                  
007000*** END OF VILMAII-COPY LENGTH= 1033 BYTES                                
