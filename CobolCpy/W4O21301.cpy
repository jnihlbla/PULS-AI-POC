000100 01  MOD-W4O21301.                                                        
000200*                                 MOD AREA FÖR SVARSPROGRAM               
000300*                                                                         
000400     03 MOD-IDTRANS1-ATTR    PIC X(2).                                    
000500*                                 MFS ATTRIBUTFÄLT                        
000600     03 MOD-IDTRANS1         PIC X.                                       
000700     03 MOD-IDTRANS2-ATTR    PIC X(2).                                    
000800*                                 MFS ATTRIBUTFÄLT                        
000900     03 MOD-IDTRANS2         PIC X.                                       
001000     03 MOD-IDTRANS3-ATTR    PIC X(2).                                    
001100*                                 MFS ATTRIBUTFÄLT                        
001200     03 MOD-IDTRANS3         PIC X.                                       
001300     03 MOD-IDTRANS4-ATTR    PIC X(2).                                    
001400*                                 MFS ATTRIBUTFÄLT                        
001500     03 MOD-IDTRANS4         PIC X.                                       
001600     03 MOD-TEMFSFEL         PIC X(40).                                   
001700*                                 MFS FELMEDDELANDE                       
001800     03 MOD-IDDISTR-IN-ATTR  PIC X(2).                                    
001900*                                 MFS ATTRIBUTFÄLT                        
002000     03 MOD-IDDISTR-IN       PIC X(4).                                    
002100*                                 DISTRIKTNUMMER                          
002200     03 MOD-IDKUNDNR-IN-ATTR PIC X(2).                                    
002300*                                 MFS ATTRIBUTFÄLT                        
002400     03 MOD-IDKUNDNR-IN      PIC X(6).                                    
002500*                                 KUNDNUMMER                              
002600     03 MOD-IDORDNR-IN-ATTR  PIC X(2).                                    
002700*                                 MFS ATTRIBUTFÄLT                        
002800     03 MOD-IDORDNR-IN       PIC X(5).                                    
002900*                                 ORDERNUMMER                             
003000     03 MOD-FLANNULL-ATTR    PIC X(2).                                    
003100*                                 MFS ATTRIBUTFÄLT                        
003200     03 MOD-FLANNULL         PIC X.                                       
003300*                                 ANNULLATION                             
003400     03 MOD-IDDISTR-UT       PIC X(4).                                    
003500*                                 DISTRIKTNUMMER                          
003600     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
003700*                                 KUNDNUMMER                              
003800     03 MOD-IDORDNR-UT       PIC X(5).                                    
003900*                                 ORDERNUMMER                             
004000     03 MOD-KDORDKL-UT       PIC 9.                                       
004100*                                 ORDERKLASS                              
004200     03 MOD-IDARTNR-NEXT     PIC 9(9).                                    
004300*                                 ARTIKELNUMMER                           
004400     03 MOD-IDLOPNR-NEXT     PIC 9(3).                                    
004500*                                 LÖPNUMMER                               
004600     03 MOD-IDSEKVNR-NEXT    PIC 9(3).                                    
004700*                                 GENERELLT SEKVENSNUMMER                 
004800     03 MOD-IDDC-NEXT        PIC X(2).                                    
004900*                                 IDENTIFIERARE LAGER                     
005000     03 MOD-KDORDBEK-NEXT    PIC 9(2).                                    
005100*                                 ORDERBEKRÄFTELSEKOD                     
005200     03 MOD-TEDDI            PIC X(11).                                   
005300*                                 TEXTFÄLT DDI                            
005400     03 MOD-RAD              OCCURS 13 TIMES.                             
005500        05 MOD-KDORDBEK-ATTR PIC X(2).                                    
005600*                                 MFS ATTRIBUTFÄLT                        
005700        05 MOD-KDORDBEK      PIC 9(2).                                    
005800*                                 ORDERBEKRÄFTELSEKOD                     
005900        05 MOD-ASTERIX       PIC X.                                       
006000        05 MOD-KDBEHX-ATTR   PIC X(2).                                    
006100*                                 MFS ATTRIBUTFÄLT                        
006200        05 MOD-KDBEHX        PIC X.                                       
006300*                                 BEHANDLINGSKOD-X                        
006400        05 MOD-IDARTNR.                                                   
006500           07 MOD-IDARTNR-1--9                                            
006600                             PIC Z(8)9.                                   
006700*                                 ARTIKELNUMMER                           
006800           07 MOD-STRAEK     PIC X.                                       
006900           07 MOD-REKSIFFR   PIC 9.                                       
007000*                                 KONTROLLSIFFRA                          
007100        05 MOD-BEART         PIC X(15).                                   
007200*                                 ARTIKELBENÄMNING      BEART-002         
007300        05 MOD-IDDC-ATTR     PIC X(2).                                    
007400*                                 MFS ATTRIBUTFÄLT                        
007500        05 MOD-IDDC-RAD      PIC X(2).                                    
007600*                                 IDENTIFIERARE LAGER                     
007700        05 MOD-KVANTAL-ATTR  PIC X(2).                                    
007800*                                 MFS ATTRIBUTFÄLT                        
007900        05 MOD-KVANTAL       PIC Z(7).                                    
008000*                                 ANTAL                                   
008100        05 MOD-KVQPACK-ATTR  PIC X(2).                                    
008200*                                 MFS ATTRIBUTFÄLT                        
008300        05 MOD-KVQPACK       PIC Z(5).                                    
008400*                                 ANTAL KVANTITETFÖRPACKNINGAR            
008500        05 MOD-IDKUNDRF-RO-ATTR                                           
008600                             PIC X(2).                                    
008700*                                 MFS ATTRIBUTFÄLT                        
008800        05 MOD-IDKUNDRF-RO   PIC Z(6)9.                                   
008900*                                 ORDERNUMMER                             
009000        05 MOD-KEYS          PIC X(18).                                   
009100     03 MOD-TEMFSINF         PIC X(55).                                   
009200*                                 INFORMATIONSMEDDELANDE                  
009300*** END OF VILMAII-COPY LENGTH= 1230 BYTES                                
