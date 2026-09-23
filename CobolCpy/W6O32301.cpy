000100 01  MOD-W6O32301.                                                        
000200*                                                                         
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-KDARBTYP-IN      PIC X(8).                                    
000800*                                 TYP AV ARBETE                           
000900     03 MOD-KDARBTYP-UT      PIC X(8).                                    
001000*                                 TYP AV ARBETE                           
001100     03 MOD-IDDC-IN          PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300     03 MOD-IDDC-UT          PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 MOD-IDARTNR-IN       PIC Z(8)9.                                   
001600*                                 ARTIKELNUMMER                           
001700     03 MOD-IDARTNR-UT       PIC Z(8)9.                                   
001800*                                 ARTIKELNUMMER                           
001900     03 MOD-FLKLAR-ATTR      PIC X(2).                                    
002000*                                 MFS ATTRIBUTFÄLT                        
002100     03 MOD-FLKLAR           PIC X.                                       
002200*                                 AVSLUTNINGSMARKERING                    
002300     03 MOD-TABELLRAD        OCCURS 12 TIMES.                             
002400*                                 GRUPP MED TABELLRADER                   
002500        05 MOD-CMD-ATTR      PIC X(2).                                    
002600*                                 MFS ATTRIBUTFÄLT                        
002700        05 MOD-CMD-IN        PIC X(2).                                    
002800*                                 MFS BEHANDLING AV INPUTFÄLT             
002900        05 MOD-IDARTNR       PIC Z(8)9.                                   
003000*                                 ARTIKELNUMMER                           
003100        05 MOD-KVANTAL       PIC Z(7).                                    
003200*                                 ANTAL                                   
003300        05 MOD-SUBEL         PIC Z(6)9.                                   
003400*                                 SUMMABELOPP                             
003500        05 MOD-IDDC          PIC X(2).                                    
003600*                                 IDENTIFIERARE LAGER                     
003700        05 MOD-BEANST        PIC X(25).                                   
003800*                                 ANSTÄLLDS NAMN                          
003900        05 MOD-IDUSER        PIC X(8).                                    
004000*                                 ANVÄNDARENS SÄKERHETS ID                
004100        05 MOD-TIDATUM       PIC 9(6).                                    
004200*                                 DATUM ENLIGT KDDATFORM                  
004300        05 MOD-FLCLASS       PIC X.                                       
004400*                                 ALLMÄN FLAGGA                           
004500        05 MOD-FLTEXT        PIC X.                                       
004600*                                 ALLMÄN FLAGGA                           
004700     03 MOD-TEMFSINF         PIC X(55).                                   
004800*                                 INFORMATIONSMEDDELANDE                  
004900*** END OF VILMAII-COPY LENGTH= 980 BYTES                                 
