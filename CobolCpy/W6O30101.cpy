000100 01  MOD-W6O30101.                                                        
000200*                                                                         
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-TIFAKT-IN        PIC X(6).                                    
000800*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
000900     03 MOD-TIFAKT-UT        PIC X(6).                                    
001000*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
001100     03 MOD-IDLBBET-IN       PIC X(12).                                   
001200*                                 LASTBÄRARBETECKNING                     
001300     03 MOD-IDLBBET-UT       PIC X(12).                                   
001400*                                 LASTBÄRARBETECKNING                     
001500     03 MOD-IDDC-SEND-IN     PIC X(2).                                    
001600*                                 SÄNDANDE LAGER                          
001700     03 MOD-IDDC-SEND-UT     PIC X(2).                                    
001800*                                 SÄNDANDE LAGER                          
001900     03 MOD-IDDC-IN          PIC X(2).                                    
002000*                                 IDENTIFIERARE LAGER                     
002100     03 MOD-IDDC-UT          PIC X(2).                                    
002200*                                 IDENTIFIERARE LAGER                     
002300     03 MOD-TIBERANK-ENTER   PIC 9(6).                                    
002400*                                 BERÄKNAD ANKOMSTDATUM                   
002500     03 MOD-TIBERANK-NEXT    PIC 9(6).                                    
002600*                                 BERÄKNAD ANKOMSTDATUM                   
002700     03 MOD-IDFAKT-ENTER     PIC Z(6)9.                                   
002800*                                 FAKTURANUMMER                           
002900     03 MOD-IDFAKT-NEXT      PIC Z(6)9.                                   
003000*                                 FAKTURANUMMER                           
003100     03 MOD-IDDC-ENTER       PIC X(2).                                    
003200*                                 IDENTIFIERARE LAGER                     
003300     03 MOD-IDDC-NEXT        PIC X(2).                                    
003400*                                 IDENTIFIERARE LAGER                     
003500     03 MOD-RUBKOL01         PIC X(19).                                   
003600     03 MOD-RUBKOL04         PIC X(3).                                    
003700     03 MOD-RUBKOL05         PIC X(7).                                    
003800     03 MOD-RUBKOL06         PIC X(4).                                    
003900     03 MOD-TABELLRAD        OCCURS 13 TIMES.                             
004000*                                 GRUPP MED TABELLRADER                   
004100        05 MOD-CMD-ATTR      PIC X(2).                                    
004200*                                 MFS ATTRIBUTFÄLT                        
004300        05 MOD-CMD-IN        PIC X(3).                                    
004400        05 MOD-IDDC-SEND     PIC X(2).                                    
004500*                                 SÄNDANDE LAGER                          
004600        05 MOD-IDDC-LEV      PIC X(2).                                    
004700*                                 LEVERERANDE DC I EXPORTFLÖDET           
004800        05 MOD-IDFAKT        PIC Z(6)9.                                   
004900*                                 FAKTURANUMMER                           
005000        05 MOD-TIFAKT        PIC 9(6).                                    
005100*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
005200        05 MOD-TIBERANK-ATTR PIC X(2).                                    
005300*                                 MFS ATTRIBUTFÄLT                        
005400        05 MOD-TIBERANK      PIC 9(6).                                    
005500*                                 BERÄKNAD ANKOMSTDATUM                   
005600        05 MOD-IDLBBET-KDTRPSTA                                           
005700                             PIC X(13).                                   
005800        05 MOD-ADINLOMR-ATTR PIC X(2).                                    
005900*                                 MFS ATTRIBUTFÄLT                        
006000        05 MOD-ADINLOMR      PIC X(4).                                    
006100*                                 INLEVERANSOMRÅDE                        
006200        05 MOD-KVKOLLI-FAKT  PIC Z(3)9.                                   
006300*                                 ANTAL FAKTURERADE KOLLIN                
006400        05 MOD-KVKOLLI-MOT   PIC Z(3)9.                                   
006500*                                 ANTAL MOTTAGNA KOLLIN                   
006600        05 MOD-KVRADER-FAKT  PIC Z(4)9.                                   
006700*                                 ANTAL RADER PER FAKTURA                 
006800        05 MOD-KVRADER-MOT   PIC Z(4)9.                                   
006900*                                 ANTAL MOTTAGNA  RADER                   
007000        05 MOD-KVRADER-PRIO  PIC Z(4)9.                                   
007100*                                 ANTAL PRIORITERADE RADER                
007200     03 MOD-ADINLOMR-PRT-ATTR                                             
007300                             PIC X(2).                                    
007400*                                 MFS ATTRIBUTFÄLT                        
007500     03 MOD-ADINLOMR-PRT     PIC X(4).                                    
007600*                                 PRINTERPLACERING                        
007700     03 MOD-TEMFSINF         PIC X(55).                                   
007800*                                 INFORMATIONSMEDDELANDE                  
007900*** END OF VILMAII-COPY LENGTH= 1148 BYTES                                
