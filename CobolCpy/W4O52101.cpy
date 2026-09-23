000100 01  MOD-W4O52101.                                                        
000200*                                 MODCOPYTEXT TILL W40521.                
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-KDPERSON-IN      PIC X(3).                                    
000800*                                 PERSONKOD                               
000900     03 MOD-KDPERSON-UT      PIC X(3).                                    
001000*                                 PERSONKOD                               
001100     03 MOD-KDFRAKT-IN       PIC X(2).                                    
001200*                                 FRAKTSÄTT C1-C2 TILL KUND               
001300     03 MOD-KDFRAKT-UT       PIC X(2).                                    
001400*                                 FRAKTSÄTT C1-C2 TILL KUND               
001500     03 MOD-IDDISTR-IN       PIC X(4).                                    
001600*                                 DISTRIKTNUMMER                          
001700     03 MOD-IDDISTR-UT       PIC X(4).                                    
001800*                                 DISTRIKTNUMMER                          
001900     03 MOD-IDKUNDNR-IN      PIC X(6).                                    
002000*                                 KUNDNUMMER                              
002100     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
002200*                                 KUNDNUMMER                              
002300     03 MOD-IDDC-IN          PIC X(2).                                    
002400*                                 IDENTIFIERARE LAGER                     
002500     03 MOD-IDDC-UT          PIC X(2).                                    
002600*                                 IDENTIFIERARE LAGER                     
002700     03 MOD-FLUTSKR-IN       PIC X.                                       
002800*                                 ORDER SOM ÄR UTSKRIVNA I FAKT           
002900     03 MOD-FLUTSKR-UT       PIC X.                                       
003000*                                 ORDER SOM ÄR UTSKRIVNA I FAKT           
003100     03 MOD-SPARADE-NYCKLAR.                                              
003200*                                 INNEHÅLLER SPARADE NYCKLAR              
003300        05 MOD-IDPRODNR-SPAR PIC 9(7).                                    
003400*                                 PRODUKTIONSNUMMER                       
003500        05 MOD-KDORDSTA-SPAR PIC 9.                                       
003600*                                 VOLVOORDERSTATUS                        
003700        05 MOD-TIBEGPAC-SPAR PIC 9(6).                                    
003800*                                 BEGÄRD PACKNINGSDAG    (ÅÅMMDD)         
003900        05 MOD-IDDISTR-SPAR  PIC 9(4).                                    
004000*                                 DISTRIKTNUMMER                          
004100        05 MOD-IDKUNDNR-SPAR PIC 9(6).                                    
004200*                                 KUNDNUMMER                              
004300        05 MOD-KDFRAKT-SPAR  PIC 9(2).                                    
004400*                                 FRAKTSÄTT C1-C2 TILL KUND               
004500     03 MOD-RAD              OCCURS 13 TIMES.                             
004600*                                 TABELL INNEHÅLLANDE RADER.              
004700        05 MOD-IDDISTR       PIC Z(3)9.                                   
004800*                                 DISTRIKTNUMMER                          
004900        05 MOD-IDKUNDNR      PIC Z(5)9.                                   
005000*                                 KUNDNUMMER                              
005100        05 MOD-KDFRAKT       PIC Z9.                                      
005200*                                 FRAKTSÄTT C1-C2 TILL KUND               
005300        05 MOD-KDORDKL       PIC 9.                                       
005400*                                 ORDERKLASS                              
005500        05 MOD-IDORDNR       PIC Z(4)9.                                   
005600*                                 ORDERNUMMER                             
005700        05 MOD-IDPRODNR      PIC Z(6)9.                                   
005800*                                 PRODUKTIONSNUMMER                       
005900        05 MOD-KVORDRAD      PIC Z(4)9.                                   
006000*                                 ANTAL ORDERRADER                        
006100        05 MOD-KVORDRAD-PACK PIC Z(4)9.                                   
006200*                                 ANTAL PACKADE ORDERRADER                
006300        05 MOD-KVKOLLI       PIC Z(3)9.                                   
006400*                                 ANTAL KOLLI                             
006500        05 MOD-TIBEGPAC      PIC 9(6).                                    
006600*                                 BEGÄRD PACKNINGSDAG    (ÅÅMMDD)         
006700        05 MOD-KVKOLLI-FAKT  PIC Z(3)9.                                   
006800*                                 ANTAL FAKTURERADE KOLLIN                
006900        05 MOD-KVKOLLI-LAST  PIC Z(3)9.                                   
007000*                                 ANTAL LASTNINGSRAPPORTERADE             
007100*                                 KOLLIN                                  
007200     03 MOD-TEMFSINF         PIC X(55).                                   
007300*                                 INFORMATIONSMEDDELANDE                  
007400*** END OF VILMAII-COPY LENGTH= 850 BYTES                                 
