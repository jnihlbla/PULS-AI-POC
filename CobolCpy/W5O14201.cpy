000100 01  MOD-W5O14201.                                                        
000200*                                 MOD-COPYTEXT FÖR W5O142                 
000300*                                                                         
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDARTNR-IN       PIC X(9).                                    
000900*                                 ARTIKELNUMMER                           
001000     03 MOD-IDARTNR-UT       PIC X(9).                                    
001100*                                 ARTIKELNUMMER                           
001200     03 MOD-IDARTNR-SPAR     PIC X(9).                                    
001300*                                 ARTIKELNUMMER                           
001400     03 MOD-IDARTNR-SPAR-RAD1                                             
001500                             PIC X(9).                                    
001600*                                 ARTIKELNUMMER                           
001700     03 MOD-RAD              OCCURS 12 TIMES                              
001800                             INDEXED MOD-IX.                              
001900*                                  TABELL-RADER                           
002000*                                                                         
002100        05 MOD-IDARTNR-ATTR  PIC X(2).                                    
002200*                                 MFS ATTRIBUTFÄLT                        
002300        05 MOD-IDARTNR-RAD   PIC Z(9).                                    
002400*                                 ARTIKELNUMMER                           
002500        05 MOD-BEFT-ATTR     PIC X(2).                                    
002600*                                 MFS ATTRIBUTFÄLT                        
002700        05 MOD-BEFT-RAD      PIC Z(2)9.                                   
002800*                                 FÖRPACKNINGSTYP                         
002900        05 MOD-PRDIRLON-ATTR PIC X(2).                                    
003000*                                 MFS ATTRIBUTFÄLT                        
003100        05 MOD-PRDIRLON-RAD  PIC Z(3)9.9(3).                              
003200*                                 DIREKT LÖN                              
003300        05 MOD-PRDMTRL-ATTR  PIC X(2).                                    
003400*                                 MFS ATTRIBUTFÄLT                        
003500        05 MOD-PRDMTRL-RAD   PIC Z(5)9.9(3).                              
003600*                                 DIREKT MATERIAL                         
003700        05 MOD-PROVRPAL-ATTR PIC X(2).                                    
003800*                                 MFS ATTRIBUTFÄLT                        
003900        05 MOD-PROVRPAL-RAD  PIC Z(3)9.9(3).                              
004000*                                 ÖVRIGA OMKOSTNADER PÅLÄGG               
004100        05 MOD-FLFPTILL-ATTR PIC X(2).                                    
004200*                                 MFS ATTRIBUTFÄLT                        
004300        05 MOD-FLFPTILL-RAD  PIC X.                                       
004400*                                 FÖRPACKN. PRISTILLÄGGS FLAGGA           
004500     03 MOD-IDARTNR-ATTR-UPP PIC X(2).                                    
004600*                                 MFS ATTRIBUTFÄLT                        
004700     03 MOD-IDARTNR-UPP      PIC X(9).                                    
004800*                                 ARTIKELNUMMER                           
004900     03 MOD-PRDIRLON-ATTR-UPP                                             
005000                             PIC X(2).                                    
005100*                                 MFS ATTRIBUTFÄLT                        
005200     03 MOD-PRDIRLON-UPP     PIC X(8).                                    
005300*                                 DIREKT LÖN                              
005400     03 MOD-PRDMTRL-ATTR-UPP PIC X(2).                                    
005500*                                 MFS ATTRIBUTFÄLT                        
005600     03 MOD-PRDMTRL-UPP      PIC X(10).                                   
005700*                                 DIREKT MATERIAL                         
005800     03 MOD-PROVRPAL-ATTR-UPP                                             
005900                             PIC X(2).                                    
006000*                                 MFS ATTRIBUTFÄLT                        
006100     03 MOD-PROVRPAL-UPP     PIC X(8).                                    
006200*                                 ÖVRIGA OMKOSTNADER PÅLÄGG               
006300     03 MOD-FLFPTILL-ATTR-UPP                                             
006400                             PIC X(2).                                    
006500*                                 MFS ATTRIBUTFÄLT                        
006600     03 MOD-FLFPTILL-UPP     PIC X.                                       
006700*                                 FÖRPACKN. PRISTILLÄGGS FLAGGA           
006800     03 MOD-KDCMD-ATTR       PIC X(2).                                    
006900*                                 MFS ATTRIBUTFÄLT                        
007000     03 MOD-KDCMD            PIC X.                                       
007100*                                 RAD-UPPDATERINGSKOMMANDO                
007200     03 MOD-TEMFSINF         PIC X(55).                                   
007300*                                 INFORMATIONSMEDDELANDE                  
007400*** END OF VILMAII-COPY LENGTH= 796 BYTES                                 
