000100 01  MOD-W9O34301.                                                        
000200*                                 MODCOPYTEXT TILL W9034300               
000300*                                 ORDER CONFIRMATION INQUIRY.             
000400*                                 FRÅGA ORDERBEKRÄFTELSE.                 
000500     03 MOD-IDTRANS          PIC X(4).                                    
000600*                                 BILDNUMMER                              
000700     03 MOD-IDMFSFEL         PIC X(3).                                    
000800*                                 MFS FELMEDDELANDE NUMMER                
000900     03 MOD-TIORDREG         PIC X(6).                                    
001000*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
001100     03 MOD-TIORDREG-NEXT    PIC 9(6).                                    
001200*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
001300     03 MOD-KDFRAKT-NEXT     PIC 9(2).                                    
001400*                                 FRAKTSÄTT DC TILL KUND                  
001500     03 MOD-KDORDKL-NEXT     PIC 9.                                       
001600*                                 ORDERKLASS                              
001700     03 MOD-IDORDNR7-NEXT    PIC 9(7).                                    
001800*                                 ORDERNUMMER                             
001900     03 MOD-IDARTNR-NEXT     PIC 9(9).                                    
002000*                                 ARTIKELNUMMER                           
002100     03 MOD-IDLOPNR-NEXT     PIC 9(3).                                    
002200*                                 LÖPNUMMER                               
002300     03 MOD-IDSEKVNR-NEXT    PIC 9(3).                                    
002400*                                 GENERELLT SEKVENSNUMMER                 
002500     03 MOD-IDDC-NEXT        PIC X(2).                                    
002600*                                 IDENTIFIERARE LAGER                     
002700     03 MOD-IDORDER-NEXT     PIC 9(7).                                    
002800*                                 VOLVO PARTS ORDERNUMMER                 
002900     03 MOD-KDORDBEK-NEXT    PIC 9(2).                                    
003000*                                 ORDERBEKRÄFTELSEKOD                     
003100     03 MOD-RAD              OCCURS 13 TIMES.                             
003200        05 MOD-IDORDNR7-RAD  PIC 9(7).                                    
003300*                                 ORDERNUMMER                             
003400        05 MOD-BEERS         PIC X(20).                                   
003500*                                 ERSÄTTNINGSTEXT                         
003600        05 MOD-IDARTNR-RAD   PIC 9(9).                                    
003700*                                 ARTIKELNUMMER                           
003800        05 MOD-KVBEART       PIC 9(6).                                    
003900*                                 BESTÄLLT ANTAL STYCKEN                  
004000        05 MOD-KVAVBART      PIC 9(6).                                    
004100*                                 AVBOKAT ANTAL ARTIKLAR                  
004200        05 MOD-IDDC          PIC X(2).                                    
004300*                                 IDENTIFIERARE LAGER                     
004400        05 MOD-KDORDBEK      PIC 9(2).                                    
004500*                                 ORDERBEKRÄFTELSEKOD                     
004600        05 MOD-TIDISPIN      PIC 9(6).                                    
004700*                                 DISP-DATUM NÄSTA INLEV (ÅÅMMDD)         
004800        05 MOD-IDORDNR7-RO   PIC 9(7).                                    
004900*                                 ORDERNUMMER                             
005000        05 MOD-IDBIL.                                                     
005100*                                 BILIDENTITET                            
005200           07 MOD-IDBILTYP   PIC X(3).                                    
005300*                                 BILTYP                                  
005400           07 MOD-TIAAAA     PIC X(4).                                    
005500*                                 ÅRTAL (ÅÅÅÅ)                            
005600           07 MOD-IDCHASSI-PIE                                            
005700                             PIC X(6).                                    
005800*                                 CHASSINUMMER PIE                        
005900        05 MOD-BERADREF      PIC X(10).                                   
006000*                                 KUNDENS RADREFERENS                     
006100*** END OF VILMAII-COPY LENGTH= 1199 BYTES                                
