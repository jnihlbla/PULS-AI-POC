000100 01  MOD-W2O31701.                                                        
000200*                                 MOD-COPYTEXT TILL W2031700              
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-IDDC-IN          PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 MOD-IDARTNR-UT       PIC X(9).                                    
001200*                                 ARTIKELNUMMER                           
001300     03 MOD-IDDC-UT          PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 MOD-CAMP-RAD         OCCURS 14 TIMES.                             
001600        05 MOD-KDCMD-ATTR    PIC X(2).                                    
001700*                                 MFS ATTRIBUTFÄLT                        
001800        05 MOD-KDCMD         PIC X.                                       
001900*                                 RAD-UPPDATERINGSKOMMANDO                
002000*                                  BLANK  = INGENTING                     
002100*                                  D , B  = DELETE                        
002200*                                  R , Ä  = REPLACE                       
002300*                                  I,N,A  = INSERT                        
002400*                                  S , V  = SELECT                        
002500*                                  P , P  = PRINT                         
002600*                                  C , K  = COPY                          
002700        05 MOD-IDDC          PIC X(2).                                    
002800*                                 IDENTIFIERARE LAGER                     
002900        05 MOD-IDKAMPRF-ATTR PIC X(2).                                    
003000*                                 MFS ATTRIBUTFÄLT                        
003100        05 MOD-IDKAMPRF      PIC Z(6)9.                                   
003200*                                 KAMPANJREFERENS                         
003300        05 MOD-KVBEART-KAMP  PIC Z(5)9.                                   
003400*                                 BESTÄLLT ANTAL FÖR KAMPANJEN            
003500        05 MOD-TIRES         PIC X(6).                                    
003600*                                 RESERVATIONSDATUM                       
003700        05 MOD-KVRESS-KAMP   PIC Z(6)9.                                   
003800*                                 TOTALT RESERVERAT FÖR KAMPANJ           
003900        05 MOD-KVRESS-REMAIN PIC Z(6)9.                                   
004000*                                 TOTALT RESERVERAT FÖR KAMPANJ           
004100        05 MOD-KVBEART-KUND  PIC Z(5)9.                                   
004200*                                 AV KUND BESTÄLLT KVANTITET              
004300        05 MOD-TISTADAT      PIC X(6).                                    
004400*                                 GENERELLT STARTDATUM                    
004500        05 MOD-TISTODAT      PIC X(6).                                    
004600*                                 GENERELLT STOPPDATUM                    
004700     03 MOD-TEMFSINF         PIC X(55).                                   
004800*                                 INFORMATIONSMEDDELANDE                  
004900*** END OF VILMAII-COPY LENGTH= 933 BYTES                                 
