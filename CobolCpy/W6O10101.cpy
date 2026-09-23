000100 01  MOD-W6O10101.                                                        
000200*                                 MOD-COPYTEXT FÖR W6010100               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDINLVGN-IN      PIC X(3).                                    
000800*                                 VAGNSIDENTITET                          
000900     03 MOD-IDDC-IN          PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 MOD-IDINLVGN-UT      PIC X(3).                                    
001200*                                 VAGNSIDENTITET                          
001300     03 MOD-IDDC-UT          PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 MOD-IDINLVGN-ENTER   PIC 9(3).                                    
001600*                                 VAGNSIDENTITET                          
001700     03 MOD-IDINLVGN-NEXT    PIC 9(3).                                    
001800*                                 VAGNSIDENTITET                          
001900     03 MOD-TABELLRAD        OCCURS 12 TIMES.                             
002000*                                 GRUPP MED TABELLRADER                   
002100        05 MOD-IDINLVGN-ATTR PIC X(2).                                    
002200*                                 MFS ATTRIBUTFÄLT                        
002300        05 MOD-IDINLVGN      PIC Z(3).                                    
002400*                                 VAGNSIDENTITET                          
002500        05 MOD-ADINLOMR-ATTR PIC X(2).                                    
002600*                                 MFS ATTRIBUTFÄLT                        
002700        05 MOD-ADINLOMR      PIC X(4).                                    
002800*                                 INLEVERANSOMRÅDE                        
002900        05 MOD-ADINLOMR-NXT-ATTR                                          
003000                             PIC X(2).                                    
003100*                                 MFS ATTRIBUTFÄLT                        
003200        05 MOD-ADINLOMR-NXT  PIC X(4).                                    
003300*                                 INLEVERANSOMRÅDE NÄSTA                  
003400     03 MOD-INDATA.                                                       
003500*                                 INDATAFÄLT                              
003600        05 MOD-KDCMDVAL-UPP-ATTR                                          
003700                             PIC X(2).                                    
003800*                                 MFS ATTRIBUTFÄLT                        
003900        05 MOD-KDCMDVAL-UPP  PIC X(3).                                    
004000*                                 GENERELL KOMMANDOKOD                    
004100        05 MOD-IDINLVGN-UPP-ATTR                                          
004200                             PIC X(2).                                    
004300*                                 MFS ATTRIBUTFÄLT                        
004400        05 MOD-IDINLVGN-UPP  PIC 9(3).                                    
004500*                                 VAGNSIDENTITET                          
004600        05 MOD-ADINLOMR-UPP-ATTR                                          
004700                             PIC X(2).                                    
004800*                                 MFS ATTRIBUTFÄLT                        
004900        05 MOD-ADINLOMR-UPP  PIC X(4).                                    
005000*                                 INLEVERANSOMRÅDE                        
005100        05 MOD-ADINLOMR-NXT-UPP-ATTR                                      
005200                             PIC X(2).                                    
005300*                                 MFS ATTRIBUTFÄLT                        
005400        05 MOD-ADINLOMR-NXT-UPP                                           
005500                             PIC X(4).                                    
005600*                                 INLEVERANSOMRÅDE NÄSTA                  
005700     03 MOD-TEMFSINF         PIC X(55).                                   
005800*                                 INFORMATIONSMEDDELANDE                  
