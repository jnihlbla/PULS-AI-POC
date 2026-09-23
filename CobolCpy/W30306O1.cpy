000100 01  MOD-W30306O1.                                                        
000200*                                 MOD-COPYTEXT FOR W30306                 
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDISTR-IN       PIC X(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900     03 MOD-IDKUNDNR-IN      PIC Z(5)9.                                   
001000*                                 KUNDNUMMER                              
001100     03 MOD-KDPRSTA-IN       PIC X.                                       
001200*                                 STATUS PRISFRÅGA                        
001300     03 MOD-IDDISTR-UT       PIC Z(3)9.                                   
001400*                                 DISTRIKTNUMMER                          
001500     03 MOD-IDKUNDNR-UT      PIC Z(5)9.                                   
001600*                                 KUNDNUMMER                              
001700     03 MOD-KDPRSTA-UT       PIC X.                                       
001800*                                 STATUS PRISFRÅGA                        
001900     03 MOD-INPUT            OCCURS 13 TIMES.                             
002000*                                 RADINFORMATION                          
002100        05 MOD-KDCMD-ATTR    PIC X(2).                                    
002200*                                 MFS ATTRIBUTFÄLT                        
002300        05 MOD-KDCMD         PIC X.                                       
002400*                                 RAD-UPPDATERINGSKOMMANDO                
002500*                                  BLANK  = INGENTING                     
002600*                                  D , B  = DELETE                        
002700*                                  R , Ä  = REPLACE                       
002800*                                  I , N  = INSERT                        
002900*                                  S , V  = SELECT                        
003000*                                  P , P  = PRINT                         
003100        05 MOD-IDDISTR       PIC Z(3)9.                                   
003200*                                 DISTRIKTNUMMER                          
003300        05 MOD-IDKUNDNR      PIC Z(5)9.                                   
003400*                                 KUNDNUMMER                              
003500        05 MOD-KDPRSTA       PIC X.                                       
003600*                                 STATUS PRISFRÅGA                        
003700        05 MOD-IDARTNR       PIC Z(8)9.                                   
003800*                                 ARTIKELNUMMER                           
003900        05 MOD-IDORDNR7      PIC Z(6)9.                                   
004000*                                 ORDERNUMMER                             
004100        05 MOD-KDVALISO      PIC X(3).                                    
004200*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
004300        05 MOD-PRARTBTO-LOC-ATTR                                          
004400                             PIC X(2).                                    
004500*                                 MFS ATTRIBUTFÄLT                        
004600        05 MOD-PRARTBTO-LOC  PIC Z(6)9.9(2).                              
004700*                                 PRIS I LOKAL VALUTA                     
004800        05 MOD-PRARTNTO-LOC-ATTR                                          
004900                             PIC X(2).                                    
005000*                                 MFS ATTRIBUTFÄLT                        
005100        05 MOD-PRARTNTO-LOC  PIC Z(6)9.9(2).                              
005200*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
005300        05 MOD-KDRAB-ATTR    PIC X(2).                                    
005400*                                 MFS ATTRIBUTFÄLT                        
005500        05 MOD-KDRAB         PIC X(5).                                    
005600*                                 RABATTKOD                               
005700     03 MOD-TEMFSINF         PIC X(55).                                   
005800*                                 INFORMATIONSMEDDELANDE                  
005900*** END OF VILMAII-COPY LENGTH= 953 BYTES                                 
