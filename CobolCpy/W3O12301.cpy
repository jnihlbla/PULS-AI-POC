000100 01  MOD-W3O12301.                                                        
000200*                                 MOD-COPYTEXT FÖR W30123                 
000300*                                 EXCHANGE POINT ADJUSTMENT               
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDDISTR-IN       PIC X(2).                                    
000900*                                 MFS BEHANDLING AV INPUTFÄLT             
001000     03 MOD-IDDISTR-UT       PIC X(4).                                    
001100*                                 DISTRIKTNUMMER                          
001200     03 MOD-KDEXCHA-IN       PIC X(2).                                    
001300*                                 MFS BEHANDLING AV INPUTFÄLT             
001400     03 MOD-KDEXCHA-UT       PIC X(3).                                    
001500*                                 EXCHANGE ACCOUNT CODE                   
001600     03 MOD-CMD-ATTR         PIC X(2).                                    
001700*                                 MFS ATTRIBUTFÄLT                        
001800     03 MOD-CMD              PIC X.                                       
001900     03 MOD-IDDISTR-UPD-ATTR PIC X(2).                                    
002000*                                 MFS ATTRIBUTFÄLT                        
002100     03 MOD-IDDISTR-UPD      PIC Z(3)9.                                   
002200*                                 DISTRIKTNUMMER                          
002300     03 MOD-KDEXCHA-UPD-ATTR PIC X(2).                                    
002400*                                 MFS ATTRIBUTFÄLT                        
002500     03 MOD-KDEXCHA-UPD      PIC Z(2)9.                                   
002600*                                 EXCHANGE ACCOUNT CODE                   
002700     03 MOD-IDBYTRAD-UPD-ATTR                                             
002800                             PIC X(2).                                    
002900*                                 MFS ATTRIBUTFÄLT                        
003000     03 MOD-IDBYTRAD-UPD     PIC Z(4)9.                                   
003100*                                 RADNUMMER                               
003200     03 MOD-TENOTE-UPD-ATTR  PIC X(2).                                    
003300*                                 MFS ATTRIBUTFÄLT                        
003400     03 MOD-TENOTE-UPD       PIC X(40).                                   
003500*                                 NOTERINGSFÄLT                           
003600     03 MOD-KVPOINT-UPD-ATTR PIC X(2).                                    
003700*                                 MFS ATTRIBUTFÄLT                        
003800     03 MOD-KVPOINT-UPD      PIC Z(6)9.                                   
003900*                                 POINT VALUE                             
004000     03 MOD-FLDEBCRE-UPD-ATTR                                             
004100                             PIC X(2).                                    
004200*                                 MFS ATTRIBUTFÄLT                        
004300     03 MOD-FLDEBCRE-UPD     PIC X.                                       
004400*                                 ALLMÄN FLAGGA                           
004500     03 MOD-TABELLRAD        OCCURS 12 TIMES.                             
004600*                                 GRUPP MED TABELL RADER                  
004700        05 MOD-IDDISTR       PIC Z(3)9.                                   
004800*                                 DISTRIKTNUMMER                          
004900        05 MOD-KDEXCHA       PIC Z(2)9.                                   
005000*                                 EXCHANGE ACCOUNT CODE                   
005100        05 MOD-IDBYTRAD      PIC Z(4)9.                                   
005200*                                 RADNUMMER                               
005300        05 MOD-TENOTE        PIC X(30).                                   
005400        05 MOD-KVPOINT       PIC Z(6)9-.                                  
005500*                                 POINT VALUE                             
005600        05 MOD-IDUSER        PIC X(8).                                    
005700*                                 ANVÄNDARENS SÄKERHETS ID                
005800        05 MOD-DAREGDAT      PIC 9(8).                                    
005900*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
006000     03 MOD-TEMFSINF         PIC X(55).                                   
006100*                                 INFORMATIONSMEDDELANDE                  
006200*** END OF VILMAII-COPY LENGTH= 977 BYTES                                 
