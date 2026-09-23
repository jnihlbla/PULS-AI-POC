000100 01  MOD-W1O12101-CTX.                                                    
000200     03 MOD-IDTRANS          PIC X(4).                                    
000300*                                 BILDNUMMER                              
000400     03 MOD-TEMFSFEL         PIC X(40).                                   
000500*                                 MFS FELMEDDELANDE                       
000600     03 MOD-BEART-ATTR       PIC X(2).                                    
000700*                                 MFS ATTRIBUTFÄLT                        
000800     03 MOD-BEART-IN         PIC X(25).                                   
000900*                                 ARTIKELBENÄMNING                        
001000     03 MOD-BEART-UT         PIC X(25).                                   
001100*                                 ARTIKELBENÄMNING                        
001200     03 MOD-IDSKYLT-SOEK-ATTR                                             
001300                             PIC X(2).                                    
001400*                                 MFS ATTRIBUTFÄLT                        
001500     03 MOD-IDSKYLT-SOEK-IN  PIC X(3).                                    
001600*                                 NATIONALITETSTECKEN                     
001700*                                 SPRÅKIDENTIFIKATION                     
001800     03 MOD-IDSKYLT-SOEK-UT  PIC X(3).                                    
001900*                                 NATIONALITETSTECKEN                     
002000*                                 SPRÅKIDENTIFIKATION                     
002100     03 MOD-IDSKYLT-SVAR-ATTR                                             
002200                             PIC X(2).                                    
002300*                                 MFS ATTRIBUTFÄLT                        
002400     03 MOD-IDSKYLT-SVAR-IN  PIC X(3).                                    
002500*                                 NATIONALITETSTECKEN                     
002600*                                 SPRÅKIDENTIFIKATION                     
002700     03 MOD-IDSKYLT-SVAR-UT  PIC X(3).                                    
002800*                                 NATIONALITETSTECKEN                     
002900*                                 SPRÅKIDENTIFIKATION                     
003000     03 MOD-KDSOEK-ATTR      PIC X(2).                                    
003100*                                 MFS ATTRIBUTFÄLT                        
003200     03 MOD-KDSOEK-IN        PIC X.                                       
003300*                                 SÖKKOD                                  
003400     03 MOD-KDSOEK-UT        PIC X.                                       
003500*                                 SÖKKOD                                  
003600     03 MOD-BEART-SPAR       PIC X(25).                                   
003700*                                 ARTIKELBENÄMNING                        
003800     03 MOD-IDBENNR-SPAR     PIC 9(7).                                    
003900*                                 BENÄMNINGSNUMMER                        
004000     03 MOD-W1O12101-001-GRP OCCURS 14 TIMES.                             
004100        05 MOD-BEART-SOEK    PIC X(25).                                   
004200*                                 ARTIKELBENÄMNING                        
004300        05 MOD-KDHOMONYM     PIC 9.                                       
004400*                                 HOMONYMKOD                              
004500        05 MOD-KDBENSTAT     PIC X.                                       
004600*                                 BENÄMNINGENS STATUS PÅ BENREG.          
004700*                                 <2 NAMNLEX, 2 RS-UNIK, 3 RENSAD         
004800        05 MOD-BEART-SVAR    PIC X(25).                                   
004900*                                 ARTIKELBENÄMNING                        
005000        05 MOD-IDBENNR       PIC Z(6)9.                                   
005100*                                 BENÄMNINGSNUMMER                        
005200     03 MOD-TEMFSINF         PIC X(55).                                   
005300*                                 INFORMATIONSMEDDELANDE                  
005400*** END OF VILMAII-COPY LENGTH= 1029 BYTES                                
