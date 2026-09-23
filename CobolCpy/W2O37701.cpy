000100 01  MOD-W2O37701.                                                        
000200*                                 MOD-COPYTEXT FÖR W20377                 
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-IDARTNR-UT       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-IDDC-IN          PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300     03 MOD-IDDC-UT          PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 MOD-UTFALT.                                                       
001600*                                 UTDATA 2377                             
001700        05 MOD-BEART-ENG     PIC X(25).                                   
001800*                                 ENGELSK ARTIKELBENÄMNING                
001900        05 MOD-TIAAVVD-DC    PIC 9(5).                                    
002000*                                 ÅR - VECKA - DAG   (ÅÅVVD)              
002100        05 MOD-FLORDSP-EJRO  PIC X.                                       
002200*                                 ORDERSPÄRR EJ RESTNOTERING              
002300        05 MOD-DAORDSP-EJRO  PIC 9(6).                                    
002400*                                 ANGER DATUM NÄR FLAGGA FÖR ORDE         
002500*                                 RSPÄRR UTAN RESTNOTERING ÄNDRAS         
002600*                                 YYYYMMDD                                
002700        05 MOD-IDUSER-ORDSP-EJRO                                          
002800                             PIC X(8).                                    
002900*                                 ANVÄNDAR-ID ORDERSPÄRR UTAN RES         
003000*                                 TNOTERING                               
003100        05 MOD-KDARTURS-DC   PIC X(2).                                    
003200*                                 ARTIKELURSPRUNGSKOD                     
003300        05 MOD-IDPSN-DC      PIC Z(2)9.                                   
003400*                                 PROPER SHIPPING NAME PER XDC            
003500        05 MOD-IDLEVNR       PIC X(5).                                    
003600*                                 LEVERANTÖRNUMMER                        
003700        05 MOD-IDPERSON-BUY  PIC Z(2)9.                                   
003800*                                 PERSONKOD REFILLANSVARIG                
003900        05 MOD-KDARTURS      PIC X(2).                                    
004000*                                 ARTIKELURSPRUNGSKOD                     
004100        05 MOD-IDPSN         PIC Z(2)9.                                   
004200*                                 PROPER SHIPPING NAME                    
004300        05 MOD-TIFINLV       PIC Z(5).                                    
004400*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
004500     03 MOD-INFALT.                                                       
004600*                                 INDATA 2377                             
004700        05 MOD-KDCMD-IN-ATTR PIC X(2).                                    
004800*                                 MFS ATTRIBUTFÄLT                        
004900        05 MOD-KDCMD-IN      PIC X.                                       
005000*                                 RAD-UPPDATERINGSKOMMANDO                
005100*                                  BLANK  = INGENTING                     
005200*                                  D , B  = DELETE                        
005300*                                  R , Ä  = REPLACE                       
005400*                                  I,N,A  = INSERT                        
005500*                                  S , V  = SELECT                        
005600*                                  P , P  = PRINT                         
005700*                                  C , K  = COPY                          
005800        05 MOD-TIAAVVD-DC-IN-ATTR                                         
005900                             PIC X(2).                                    
006000*                                 MFS ATTRIBUTFÄLT                        
006100        05 MOD-TIAAVVD-DC-IN PIC 9(5).                                    
006200*                                 ÅR - VECKA - DAG   (ÅÅVVD)              
006300        05 MOD-FLORDSP-EJRO-IN-ATTR                                       
006400                             PIC X(2).                                    
006500*                                 MFS ATTRIBUTFÄLT                        
006600        05 MOD-FLORDSP-EJRO-IN                                            
006700                             PIC X.                                       
006800*                                 ORDERSPÄRR EJ RESTNOTERING              
006900        05 MOD-KDARTURS-DC-IN-ATTR                                        
007000                             PIC X(2).                                    
007100*                                 MFS ATTRIBUTFÄLT                        
007200        05 MOD-KDARTURS-DC-IN                                             
007300                             PIC X(2).                                    
007400*                                 ARTIKELURSPRUNGSKOD                     
007500        05 MOD-IDPSN-DC-IN-ATTR                                           
007600                             PIC X(2).                                    
007700*                                 MFS ATTRIBUTFÄLT                        
007800        05 MOD-IDPSN-DC-IN   PIC 9(3).                                    
007900*                                 PROPER SHIPPING NAME PER XDC            
008000     03 MOD-TEMFSINF         PIC X(55).                                   
008100*                                 INFORMATIONSMEDDELANDE                  
008200*** END OF VILMAII-COPY LENGTH= 211 BYTES                                 
