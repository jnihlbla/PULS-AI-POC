000100 01  MOD-W4O20301.                                                        
000200*                                 MOD AREA FÖR SVARSPROGRAM               
000300*                                                                         
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDDISTR-IN       PIC X(4).                                    
000900*                                 DISTRIKTNUMMER                          
001000     03 MOD-IDKUNDNR-IN      PIC X(6).                                    
001100*                                 KUNDNUMMER                              
001200     03 MOD-IDORDNR-IN       PIC X(5).                                    
001300*                                 ORDERNUMMER                             
001400     03 MOD-IDDISTR-UT       PIC X(4).                                    
001500*                                 DISTRIKTNUMMER                          
001600     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001700*                                 KUNDNUMMER                              
001800     03 MOD-IDORDNR-UT       PIC X(5).                                    
001900*                                 ORDERNUMMER                             
002000     03 MOD-KDORDKL-UT       PIC 9.                                       
002100*                                 ORDERKLASS                              
002200     03 MOD-IDARTNR-NEXT     PIC 9(9).                                    
002300*                                 ARTIKELNUMMER                           
002400     03 MOD-IDLOPNR-NEXT     PIC 9(3).                                    
002500*                                 LÖPNUMMER                               
002600     03 MOD-IDSEKVNR-NEXT    PIC 9(3).                                    
002700*                                 GENERELLT SEKVENSNUMMER                 
002800     03 MOD-IDDC-NEXT        PIC X(2).                                    
002900*                                 IDENTIFIERARE LAGER                     
003000     03 MOD-KDORDBEK-NEXT    PIC 9(2).                                    
003100*                                 ORDERBEKRÄFTELSEKOD                     
003200     03 MOD-KDTRTYP          PIC X.                                       
003300*                                 IMS TRANSAKTIONSTYP                     
003400     03 MOD-TEDDI            PIC X(11).                                   
003500*                                 TEXTFÄLT DDI                            
003600     03 MOD-RAD              OCCURS 13 TIMES.                             
003700        05 MOD-KDORDBEK-ATTR PIC X(2).                                    
003800*                                 MFS ATTRIBUTFÄLT                        
003900        05 MOD-KDORDBEK      PIC 9(2).                                    
004000*                                 ORDERBEKRÄFTELSEKOD                     
004100        05 MOD-ASTERIX       PIC X.                                       
004200        05 MOD-KDBEHX-ATTR   PIC X(2).                                    
004300*                                 MFS ATTRIBUTFÄLT                        
004400        05 MOD-KDBEHX        PIC X.                                       
004500*                                 BEHANDLINGSKOD-X                        
004600        05 MOD-IDARTNR.                                                   
004700           07 MOD-IDARTNR-1--9                                            
004800                             PIC Z(8)9.                                   
004900*                                 ARTIKELNUMMER                           
005000           07 MOD-STRAEK     PIC X.                                       
005100           07 MOD-REKSIFFR   PIC 9.                                       
005200*                                 KONTROLLSIFFRA                          
005300        05 MOD-BEART         PIC X(15).                                   
005400*                                 ARTIKELBENÄMNING      BEART-002         
005500        05 MOD-IDDC-ATTR     PIC X(2).                                    
005600*                                 MFS ATTRIBUTFÄLT                        
005700        05 MOD-IDDC-RAD      PIC X(2).                                    
005800*                                 IDENTIFIERARE LAGER                     
005900        05 MOD-KVANTAL-ATTR  PIC X(2).                                    
006000*                                 MFS ATTRIBUTFÄLT                        
006100        05 MOD-KVANTAL       PIC Z(7).                                    
006200*                                 ANTAL                                   
006300        05 MOD-KVQPACK-ATTR  PIC X(2).                                    
006400*                                 MFS ATTRIBUTFÄLT                        
006500        05 MOD-KVQPACK       PIC Z(4)9.                                   
006600*                                 ANTAL KVANTITETFÖRPACKNINGAR            
006700        05 MOD-IDKUNDRF-RO-ATTR                                           
006800                             PIC X(2).                                    
006900*                                 MFS ATTRIBUTFÄLT                        
007000        05 MOD-IDKUNDRF-RO   PIC Z(6)9.                                   
007100*                                 ORDERNUMMER                             
007200        05 MOD-KEYS          PIC X(18).                                   
007300     03 MOD-TEMFSINF         PIC X(55).                                   
007400*                                 INFORMATIONSMEDDELANDE                  
007500*** END OF VILMAII-COPY LENGTH= 1214 BYTES                                
