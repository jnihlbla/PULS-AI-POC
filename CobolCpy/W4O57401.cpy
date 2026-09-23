000100 01  MOD-W4O57401.                                                        
000200*                                 COPYTEXT FÖR MID W4O57401               
000300*                                                                         
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDDISTR-IN       PIC X(4).                                    
000900*                                 DISTRIKTNUMMER                          
001000     03 MOD-IDDISTR-UT       PIC X(4).                                    
001100*                                 DISTRIKTNUMMER                          
001200     03 MOD-IDKUNDNR-IN      PIC X(6).                                    
001300*                                 KUNDNUMMER                              
001400     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001500*                                 KUNDNUMMER                              
001600     03 MOD-IDARTNR-IN       PIC X(9).                                    
001700*                                 ARTIKELNUMMER                           
001800     03 MOD-IDARTNR-UT       PIC X(9).                                    
001900*                                 ARTIKELNUMMER                           
002000     03 MOD-IDORDNR-IN       PIC X(5).                                    
002100*                                 ORDERNUMMER                             
002200     03 MOD-IDORDNR-UT       PIC X(5).                                    
002300*                                 ORDERNUMMER                             
002400     03 MOD-SPARADE-NYCKLAR.                                              
002500*                                              SPARADE NYCKLAR            
002600*                                                                         
002700        05 MOD-IDDISTR-SPAR  PIC 9(4).                                    
002800*                                 DISTRIKTNUMMER                          
002900        05 MOD-IDKUNDNR-SPAR PIC 9(6).                                    
003000*                                 KUNDNUMMER                              
003100        05 MOD-IDARTNR-SPAR  PIC 9(9).                                    
003200*                                 ARTIKELNUMMER                           
003300        05 MOD-IDORDNR-SPAR  PIC 9(5).                                    
003400*                                 ORDERNUMMER                             
003500     03 MOD-RAD              OCCURS 14 TIMES                              
003600                             INDEXED MOD-IX-1.                            
003700*                                                                         
003800*                                                                         
003900        05 MOD-IDKUNDNR      PIC Z(6).                                    
004000*                                 KUNDNUMMER                              
004100        05 MOD-IDARTNR       PIC Z(7)9.                                   
004200*                                 ARTIKELNUMMER                           
004300        05 MOD-IDORDNR       PIC Z(5).                                    
004400*                                 ORDERNUMMER                             
004500        05 MOD-IDDC          PIC X(2).                                    
004600*                                 IDENTIFIERARE LAGER                     
004700        05 MOD-KVART         PIC Z(6)9.                                   
004800*                                 ANTAL ARTNR PER BRYTBEGREPP             
004900        05 MOD-IDKUNDRF-LEV  PIC X(10).                                   
005000*                                 KUND REF PÅ LEVERANSORDERN              
005100     03 MOD-SPARADE-NYCKLAR-ENT.                                          
005200*                                       SPARADE NYCKLAR ENTER             
005300*                                                                         
005400        05 MOD-IDDISTR-SPAR-E                                             
005500                             PIC 9(4).                                    
005600*                                 DISTRIKTNUMMER                          
005700        05 MOD-IDKUNDNR-SPAR-E                                            
005800                             PIC 9(6).                                    
005900*                                 KUNDNUMMER                              
006000        05 MOD-IDARTNR-SPAR-E                                             
006100                             PIC 9(9).                                    
006200*                                 ARTIKELNUMMER                           
006300        05 MOD-IDORDNR-SPAR-E                                             
006400                             PIC 9(5).                                    
006500*                                 ORDERNUMMER                             
006600     03 MOD-TEMFSINF         PIC X(55).                                   
006700*                                 INFORMATIONSMEDDELANDE                  
006800*** END OF VILMAII-COPY LENGTH= 727 BYTES                                 
