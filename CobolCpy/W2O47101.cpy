000100 01  MOD-W2O47101.                                                        
000200*                                 MOD-COPYTEXT FÖR W2047100               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-IDARTNR-UT       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-STRECK           PIC X.                                       
001200     03 MOD-REKSIFFR         PIC 9.                                       
001300*                                 KONTROLLSIFFRA                          
001400     03 MOD-IDDC-IN          PIC X(2).                                    
001500*                                 IDENTIFIERARE LAGER                     
001600     03 MOD-IDDC-UT          PIC X(2).                                    
001700*                                 IDENTIFIERARE LAGER                     
001800     03 MOD-IDANSK-IN        PIC X(3).                                    
001900*                                 ANSKAFFARNUMMER                         
002000     03 MOD-IDANSK-UT        PIC Z(2)9.                                   
002100*                                 ANSKAFFARNUMMER                         
002200     03 MOD-IDLEVNR-IN       PIC X(5).                                    
002300*                                 LEVERANTÖRNUMMER                        
002400     03 MOD-IDLEVNR-UT       PIC X(5).                                    
002500*                                 LEVERANTÖRNUMMER                        
002600     03 MOD-KDLARM-IN        PIC X(3).                                    
002700*                                 LARMORSAKSKOD                           
002800     03 MOD-KDLARM-UT        PIC X(3).                                    
002900*                                 LARMORSAKSKOD                           
003000     03 MOD-INFO-RAD         OCCURS 14 TIMES.                             
003100*                                 RADINFORMATION                          
003200        05 MOD-KDCMD-ATTR    PIC X(2).                                    
003300*                                 MFS ATTRIBUTFÄLT                        
003400        05 MOD-KDCMD         PIC X.                                       
003500*                                 RAD-UPPDATERINGSKOMMANDO                
003600*                                  BLANK  = INGENTING                     
003700*                                  D , B  = DELETE                        
003800*                                  R , Ä  = REPLACE                       
003900*                                  I,N,A  = INSERT                        
004000*                                  S , V  = SELECT                        
004100*                                  P , P  = PRINT                         
004200*                                  C , K  = COPY                          
004300        05 MOD-IDDC          PIC X(2).                                    
004400*                                 IDENTIFIERARE LAGER                     
004500        05 MOD-FLNYLARM-ATTR PIC X(2).                                    
004600*                                 MFS ATTRIBUTFÄLT                        
004700        05 MOD-FLNYLARM      PIC X(2).                                    
004800*                                 MFS BEHANDLING AV INPUTFÄLT             
004900        05 MOD-KDLARM        PIC Z(2)9.                                   
005000*                                 LARMORSAKSKOD                           
005100        05 MOD-IDARTNR       PIC Z(8)9.                                   
005200*                                 ARTIKELNUMMER                           
005300        05 MOD-IDLEVNR       PIC X(5).                                    
005400*                                 LEVERANTÖRNUMMER                        
005500        05 MOD-TISENBEK-DAG  PIC 9(5).                                    
005600*                                 ÅR - VECKA - DAG   (ÅÅVVD)              
005700        05 MOD-IDDISTR       PIC Z(3)9.                                   
005800*                                 DISTRIKTNUMMER                          
005900        05 MOD-TEORSLRM      PIC X(25).                                   
006000        05 MOD-TIREGDAT      PIC 9(6).                                    
006100*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
006200     03 MOD-TEMFSINF         PIC X(55).                                   
006300*                                 INFORMATIONSMEDDELANDE                  
006400*** END OF VILMAII-COPY LENGTH= 1069 BYTES                                
