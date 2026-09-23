000100 01  MOD-W2O42501.                                                        
000200*                                 COPYTEXT FÖR MOD W2O42501               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-IDDC-IN          PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 MOD-IDARTNR.                                                      
001200        05 MOD-IDARTNR-UT    PIC X(9).                                    
001300*                                 ARTIKELNUMMER                           
001400        05 MOD-DASH-1        PIC X.                                       
001500        05 MOD-REKSIFFR      PIC X.                                       
001600*                                 KONTROLLSIFFRA                          
001700     03 MOD-IDDC-UT          PIC X(2).                                    
001800*                                 IDENTIFIERARE LAGER                     
001900     03 MOD-LINE             OCCURS 13 TIMES.                             
002000*                                  RAD FÖR FÖRÄNDRINGAR                   
002100*                                                                         
002200        05 MOD-IDDC-RO-LINE  PIC X(2).                                    
002300*                                 LAGER DÄR RESTORDER FÅR SKE             
002400        05 MOD-IDARTNR-LINE  PIC Z(9).                                    
002500*                                 ARTIKELNUMMER                           
002600        05 MOD-IDDISTR-LINE  PIC Z(3)9.                                   
002700*                                 DISTRIKTNUMMER                          
002800        05 MOD-IDKUNDNR-LINE PIC Z(5)9.                                   
002900*                                 KUNDNUMMER                              
003000        05 MOD-IDORDNR-LINE  PIC Z(6)9.                                   
003100*                                 ORDERNUMMER                             
003200        05 MOD-KVART-LINE    PIC Z(6)9.                                   
003300*                                 ANTAL ARTNR PER BRYTBEGREPP             
003400        05 MOD-TIRODAT-LINE  PIC X(6).                                    
003500*                                 RESTORDERDATUM         (ÅÅMMDD)         
003600        05 MOD-TIRES-LINE    PIC X(6).                                    
003700*                                 RESERVATIONSDATUM                       
003800        05 MOD-KDORDKL-LINE  PIC Z.                                       
003900*                                 ORDERKLASS                              
004000        05 MOD-KDROO-LINE    PIC Z.                                       
004100*                                 KODEN ANGER VARFÖR RADEN VÄNTAR         
004200        05 MOD-IDANSK-LINE   PIC Z(2)9.                                   
004300*                                 ANSKAFFARNUMMER                         
004400        05 MOD-KDSTARAD-LINE PIC X.                                       
004500*                                 RADSTATUSKOD                            
004600        05 MOD-KDRAPRIO-LINE PIC Z(2)9.                                   
004700*                                 PRIORITETSKOD PÅ RADEN                  
004800     03 MOD-TEMFSINF         PIC X(55).                                   
004900*                                 INFORMATIONSMEDDELANDE                  
005000*** END OF VILMAII-COPY LENGTH= 851 BYTES                                 
