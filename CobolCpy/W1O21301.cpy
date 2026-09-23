000100 01  MOD-W1O21301.                                                        
000200*                                 MOD-COPYTEXT FÖR W1021300               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-IDARTNR-UT       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-IDSKYLT-IN       PIC X(3).                                    
001200*                                 NATIONALITETSTECKEN                     
001300*                                 SPRÅKIDENTIFIKATION                     
001400     03 MOD-IDSKYLT-UT       PIC X(3).                                    
001500*                                 NATIONALITETSTECKEN                     
001600*                                 SPRÅKIDENTIFIKATION                     
001700     03 MOD-IDRADNR-IN       PIC X(4).                                    
001800*                                 RADNUMMER                               
001900     03 MOD-IDRADNR-UT       PIC X(4).                                    
002000*                                 RADNUMMER                               
002100     03 MOD-IDRADNR-DOLT     PIC 9(4).                                    
002200*                                 RADNUMMER                               
002300     03 MOD-IDRADNR-DOLT2    PIC 9(4).                                    
002400*                                 RADNUMMER                               
002500     03 MOD-IDSATSNR-DOLT    PIC X(9).                                    
002600*                                 ARTIKELNUMMER                           
002700     03 MOD-BEART            PIC X(25).                                   
002800*                                 ARTIKELBENÄMNING                        
002900     03 MOD-KDPRODSL         PIC Z9.                                      
003000*                                 PRODUKTSLAG                             
003100     03 MOD-IDFKNGRP         PIC Z(3)9.                                   
003200*                                 FUNKTIONSGRUPP                          
003300     03 MOD-IDSTRTYP         PIC X.                                       
003400*                                 STRUKTURTYP                             
003500     03 MOD-KDPSLLOC         PIC 9(2).                                    
003600*                                 PRODUKTSLAG LOKALT                      
003700     03 MOD-OUTPUT           OCCURS 12 TIMES.                             
003800*                                                                         
003900        05 MOD-SELECT-ATTR   PIC X(2).                                    
004000*                                 MFS ATTRIBUTFÄLT                        
004100        05 MOD-SELECT        PIC X.                                       
004200        05 MOD-IDRADNR       PIC 9(4).                                    
004300*                                 RADNUMMER                               
004400        05 MOD-UTRAD         PIC X(72).                                   
004500        05 MOD-PART-LINE REDEFINES MOD-UTRAD.                             
004600*                                                                         
004700           07 MOD-REANTPSA-LINE                                           
004800                             PIC Z9.9(3).                                 
004900*                                 ANTAL PER SATS                          
005000           07 MOD-IDARTNR-LINE                                            
005100                             PIC Z(9).                                    
005200*                                 ARTIKELNUMMER                           
005300           07 MOD-FILLER     PIC X.                                       
005400           07 MOD-IDANSK-LINE                                             
005500                             PIC Z9(2).                                   
005600*                                 ANSKAFFARNUMMER                         
005700           07 MOD-FILLER     PIC X.                                       
005800           07 MOD-PRARTSTD-LINE                                           
005900                             PIC -(7)9.9(2).                              
006000*                                 ARTIKELSTANDARDPRIS                     
006100           07 MOD-FILLER     PIC X.                                       
006200           07 MOD-KVVECKOR-LT-LINE                                        
006300                             PIC Z(2).                                    
006400*                                 ANTAL VECKOR LEDTID                     
006500           07 MOD-FILLER     PIC X.                                       
006600           07 MOD-ARB-SALDO-LINE                                          
006700                             PIC -(7)9.                                   
006800*                                 LAGERTILLGÅNG                           
006900           07 MOD-FILLER     PIC X.                                       
007000           07 MOD-BEART-LINE PIC X(15).                                   
007100           07 MOD-FILLER     PIC X.                                       
007200           07 MOD-IDSTRTYP-LINE                                           
007300                             PIC X.                                       
007400*                                 STRUKTURTYP                             
007500           07 MOD-FILLERX2   PIC X(2).                                    
007600           07 MOD-KDISATS-LINE                                            
007700                             PIC X.                                       
007800*                                 STATUSKOD I SATS                        
007900           07 MOD-FILLER     PIC X.                                       
008000           07 MOD-TIAAVV-LINE                                             
008100                             PIC 9(4).                                    
008200*                                 ÅR - VECKA  (ÅÅVV)                      
008300           07 MOD-FILLERX2   PIC X(2).                                    
008400           07 MOD-KDFARLIG-LINE                                           
008500                             PIC X.                                       
008600*                                 KOD FÖR FARLIGT GODS                    
008700        05 MOD-SUPPL-PART-LINE REDEFINES MOD-UTRAD.                       
008800*                                                                         
008900           07 MOD-REANTPSA-LINE-S                                         
009000                             PIC Z9.9(3).                                 
009100*                                 ANTAL PER SATS                          
009200           07 MOD-FILLER     PIC X.                                       
009300           07 MOD-BELEVART-LINE-S                                         
009400                             PIC X(30).                                   
009500*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
009600           07 MOD-SUPPL-LINE-S                                            
009700                             PIC X(3).                                    
009800           07 MOD-FILLER     PIC X.                                       
009900           07 MOD-IDLEVNR-LINE-S                                          
010000                             PIC X(5).                                    
010100*                                 LEVERANTÖRNUMMER                        
010200           07 MOD-FILLER     PIC X.                                       
010300           07 MOD-BEART-LINE-S                                            
010400                             PIC X(15).                                   
010500           07 MOD-FILLER     PIC X.                                       
010600           07 MOD-IDSTRTYP-LINE-S                                         
010700                             PIC X.                                       
010800*                                 STRUKTURTYP                             
010900           07 MOD-FILLERX2   PIC X(2).                                    
011000           07 MOD-KDISATS-LINE-S                                          
011100                             PIC X.                                       
011200*                                 STATUSKOD I SATS                        
011300           07 MOD-FILLER     PIC X.                                       
011400           07 MOD-TIAAVV-LINE-S                                           
011500                             PIC 9(4).                                    
011600*                                 ÅR - VECKA  (ÅÅVV)                      
011700        05 MOD-NOTE-LINE-FILLER REDEFINES MOD-UTRAD.                      
011800           07 MOD-NOTE-LINE.                                              
011900*                                                                         
012000              09 MOD-TESTRNOT-LINE                                        
012100                             PIC X(70).                                   
012200*                                 STRUKTURNOTERING                        
012300              09 MOD-CONTINUE-LINE                                        
012400                             PIC X.                                       
012500           07 FILLER         PIC X.                                       
012600     03 MOD-KDPRTVAL-ATTR    PIC X(2).                                    
012700*                                 MFS ATTRIBUTFÄLT                        
012800     03 MOD-KDPRTVAL         PIC X.                                       
012900*                                 PRINTER-VAL KOD                         
013000     03 MOD-TEMFSINF         PIC X(55).                                   
013100*                                 INFORMATIONSMEDDELANDE                  
013200*** END OF VILMAII-COPY LENGTH= 1133 BYTES                                
