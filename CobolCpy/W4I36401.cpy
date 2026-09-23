000100 01  MID-W4I36401.                                                        
000200*                                 COPYTEXT FÖR MID W4I36401               
000300     03 MID-IDPRC-IN.                                                     
000400*                                 PRODUKTIONSKANAL                        
000500        05 MID-IDPRCBAS      PIC X(3).                                    
000600*                                 PRC-BAS                                 
000700        05 MID-IDPRCVAR      PIC X.                                       
000800*                                 PRC-VARIANT                             
000900     03 MID-IDPRC-UT.                                                     
001000*                                 PRODUKTIONSKANAL                        
001100        05 MID-IDPRCBAS      PIC X(3).                                    
001200*                                 PRC-BAS                                 
001300        05 MID-IDPRCVAR      PIC X.                                       
001400*                                 PRC-VARIANT                             
001500     03 MID-IDDC-IN          PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700     03 MID-IDDC-UT          PIC X(2).                                    
001800*                                 IDENTIFIERARE LAGER                     
001900     03 MID-FLAGGA-BORTTAG   PIC X.                                       
002000*                                 ALLMÄN FLAGGA                           
002100     03 MID-FLAGGA-SPAR      PIC X.                                       
002200*                                 ALLMÄN FLAGGA                           
002300     03 MID-FLAGGA-RAD       PIC X.                                       
002400*                                 ALLMÄN FLAGGA                           
002500     03 MID-INPUT.                                                        
002600*                                 INDATA FÖR UPPDATERING                  
002700        05 MID-IDPRC-UPDATE.                                              
002800*                                 PRODUKTIONSKANAL                        
002900           07 MID-IDPRCBAS   PIC X(3).                                    
003000*                                 PRC-BAS                                 
003100           07 MID-IDPRCVAR   PIC X.                                       
003200*                                 PRC-VARIANT                             
003300        05 MID-BEPRC-RAD     PIC X(15).                                   
003400*                                 PRODUKTIONKANALSNAMN                    
003500        05 MID-KDPRCGRP-UPDATE                                            
003600                             PIC X(5).                                    
003700*                                 PRODUKTIONSKANALSGRUPP                  
003800        05 MID-KDPRODKL-UPDATE                                            
003900                             PIC X.                                       
004000*                                 PRODUKTIONSKLASS                        
004100        05 MID-KDPRCTYP-UPDATE                                            
004200                             PIC X.                                       
004300*                                 PRODUKTIONSKANALSTYP                    
004400        05 MID-IDPRC-HUV-UPDATE.                                          
004500*                                 HUVUDPRODUKTIONSKANAL                   
004600           07 MID-IDPRCBAS-HUV                                            
004700                             PIC X(3).                                    
004800*                                 PRC-BAS                                 
004900           07 MID-IDPRCVAR-HUV                                            
005000                             PIC X.                                       
005100*                                 PRC-VARIANT                             
005200        05 MID-VLKOLGR-UPDATE                                             
005300                             PIC X(4).                                    
005400*                                 NETTOGRÄNS FÖR EGET KOLLI I M3          
005500        05 MID-IDPRC-SUB-UPDATE                                           
005600                             OCCURS 10 TIMES.                             
005700*                                 PICKUP PRODUKTIONSKANAL                 
005800           07 MID-IDPRCBAS-SUB                                            
005900                             PIC X(3).                                    
006000*                                 PRC-BAS                                 
006100           07 MID-IDPRCVAR-SUB                                            
006200                             PIC X.                                       
006300*                                 PRC-VARIANT                             
006400        05 MID-KVVTID-UPDATE PIC X(5).                                    
006500*                                 ORDER VÄNTETID I PRC (TTMM)             
006600        05 MID-RESPLIT-UPDATE                                             
006700                             PIC X(4).                                    
006800        05 MID-FLSTJORD-UPDATE                                            
006900                             PIC X.                                       
007000*                                 FLAGGA FÖR STJÄRNORDER PBV              
007100        05 MID-ADLAGOMR-UPDATE                                            
007200                             OCCURS 10 TIMES                              
007300                             PIC 9(2).                                    
007400*                                 LAGEROMRÅDE                             
007500        05 MID-KVORDER-UPDATE                                             
007600                             PIC 9(7).                                    
007700*                                 ANTAL ORDER                             
007800        05 MID-KVRADER-UPDATE                                             
007900                             PIC 9(5).                                    
008000*                                 ANTAL RADER                             
008100        05 MID-VKORDNTO-UPDATE                                            
008200                             PIC X(8).                                    
008300*                                 ORDERVIKT NETTO (KG)                    
008400        05 MID-VLORDNTO-UPDATE                                            
008500                             PIC X(8).                                    
008600*                                 ORDERVOLYM NETTO (M3)                   
008700        05 MID-KVPLSRAD-UPDATE                                            
008800                             PIC 9(5).                                    
008900*                                 ANTAL RADER                             
009000        05 MID-VKPLSNTO-UPDATE                                            
009100                             PIC X(8).                                    
009200*                                 ORDERVIKT NETTO (KG)                    
009300        05 MID-VLPLSNTO-UPDATE                                            
009400                             PIC X(8).                                    
009500*                                 ORDERVOLYM NETTO (M3)                   
009600*** END OF VILMAII-COPY LENGTH= 168 BYTES                                 
