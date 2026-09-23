000100 01  MOD-W4O23301.                                                        
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
005500        05 MOD-KDORDBEK      PIC 9(2).                                    
005600*                                 ORDERBEKRÄFTELSEKOD                     
005700        05 MOD-KDBEHX        PIC X.                                       
005800*                                 BEHANDLINGSKOD-X                        
005900        05 MOD-IDARTNR.                                                   
006000           07 MOD-IDARTNR-1--9                                            
006100                             PIC Z(8)9.                                   
006200*                                 ARTIKELNUMMER                           
006300           07 MOD-STRAEK     PIC X.                                       
006400           07 MOD-REKSIFFR   PIC 9.                                       
006500*                                 KONTROLLSIFFRA                          
006600        05 MOD-BEART         PIC X(15).                                   
006700*                                 ARTIKELBENÄMNING      BEART-002         
006800        05 MOD-IDDC-RAD      PIC X(2).                                    
006900*                                 IDENTIFIERARE LAGER                     
007000        05 MOD-KVANTAL       PIC Z(7).                                    
007100*                                 ANTAL                                   
007200        05 MOD-KEYS          PIC X(6).                                    
007300     03 MOD-TEMFSINF         PIC X(55).                                   
007400*                                 INFORMATIONSMEDDELANDE                  
007500*** END OF VILMAII-COPY LENGTH= 749 BYTES                                 
