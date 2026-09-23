000100 01  MOD-W6O30201.                                                        
000200*                                                                         
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDFAKT-IN        PIC X(7).                                    
000800*                                 FAKTURANUMMER                           
000900     03 MOD-IDFAKT-UT        PIC X(7).                                    
001000*                                 FAKTURANUMMER                           
001100     03 MOD-IDKUNDRF-ENTER   PIC X(10).                                   
001200*                                 KUNDENS REFERENS (ORDERID)              
001300     03 MOD-IDKUNDRF-NEXT    PIC X(10).                                   
001400*                                 KUNDENS REFERENS (ORDERID)              
001500     03 MOD-IDKUNDNR-ENTER   PIC Z(5)9.                                   
001600*                                 KUNDNUMMER                              
001700     03 MOD-IDKUNDNR-NEXT    PIC Z(5)9.                                   
001800*                                 KUNDNUMMER                              
001900     03 MOD-IDKOLLI-ENTER    PIC Z(4)9.                                   
002000*                                 KOLLINUMMER                             
002100     03 MOD-IDKOLLI-NEXT     PIC Z(4)9.                                   
002200*                                 KOLLINUMMER                             
002300     03 MOD-IDDC-SPAR        PIC X(2).                                    
002400*                                 IDENTIFIERARE LAGER                     
002500     03 MOD-IDUSER-ATTR      PIC X(2).                                    
002600*                                 MFS ATTRIBUTFÄLT                        
002700     03 MOD-IDUSER-003       PIC X(5).                                    
002800*                                 ANSVARIGT USERID INLÄGGN.(R32)          
002900     03 MOD-ADINLOMR-PRT-ATTR                                             
003000                             PIC X(2).                                    
003100*                                 MFS ATTRIBUTFÄLT                        
003200     03 MOD-ADINLOMR-PRT     PIC X(4).                                    
003300*                                 PRINTERPLACERING                        
003400     03 MOD-TABELLRAD        OCCURS 12 TIMES.                             
003500*                                 GRUPP MED TABELLRADER                   
003600        05 MOD-CMD-ATTR      PIC X(2).                                    
003700*                                 MFS ATTRIBUTFÄLT                        
003800        05 MOD-CMD-IN        PIC X(3).                                    
003900        05 MOD-IDKUNDRF      PIC X(10).                                   
004000*                                 KUNDENS REFERENS (ORDERID)              
004100        05 MOD-IDKUNDNR      PIC Z(5)9.                                   
004200*                                 KUNDNUMMER                              
004300        05 MOD-IDKOLLI       PIC Z(4)9.                                   
004400*                                 KOLLINUMMER                             
004500        05 MOD-KDKOLLI       PIC X(8).                                    
004600*                                 KOLLIKOD                                
004700        05 MOD-IDARTNR-KOLLI PIC Z(5)9.                                   
004800*                                 ANTAL                                   
004900        05 MOD-IDARTNR-NEW   PIC Z(6).                                    
005000*                                 ANTAL                                   
005100        05 MOD-IDARTNR-PRIO  PIC Z(6).                                    
005200*                                 ANTAL                                   
005300        05 MOD-TEINFO        PIC X(7).                                    
005400        05 MOD-ADINLOMR-ATTR PIC X(2).                                    
005500*                                 MFS ATTRIBUTFÄLT                        
005600        05 MOD-ADINLOMR      PIC X(4).                                    
005700*                                 INLEVERANSOMRÅDE                        
005800     03 MOD-TEMFSINF         PIC X(55).                                   
005900*                                 INFORMATIONSMEDDELANDE                  
006000*** END OF VILMAII-COPY LENGTH= 950 BYTES                                 
