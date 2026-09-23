000100 01  RESP-WL016800.                                                       
000200*                                 COPYTEXT FÖR MOD                        
000300*                                 WL0168O1                                
000400     03 RESP-IDDC-KEY        PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600     03 RESP-IDDC-REC-KEY    PIC X(2).                                    
000700*                                 MOTTAGANDE LAGER                        
000800     03 RESP-IDDISTR-KEY     PIC Z(5).                                    
000900*                                 DISTRIKTNUMMER                          
001000     03 RESP-IDKUNDNR-KEY    PIC Z(7).                                    
001100*                                 KUNDNUMMER                              
001200     03 RESP-IDBYTRAP-KEY    PIC Z(7).                                    
001300*                                 RAPPORTNUMMER  BYTES                    
001400     03 RESP-VKORDBTO        PIC Z(5)9.9.                                 
001500*                                 ORDERVIKT BRUTTO (KG)                   
001600     03 RESP-VLORDBTO        PIC Z(3)9.9(3).                              
001700*                                 ORDERVOLYM BRUTTO (M3)                  
001800     03 RESP-FLKLAR          PIC X.                                       
001900*                                 AVSLUTNINGSMARKERING                    
002000     03 RESP-KVRADER         PIC Z(4)9.                                   
002100*                                 ANTAL RADER                             
002200     03 RESP-DISTR-GRUPP     OCCURS 3000 TIMES.                           
002300        05 RESP-KDCMD-RAD    PIC X.                                       
002400*                                 RAD-UPPDATERINGSKOMMANDO                
002500*                                  BLANK  = INGENTING                     
002600*                                  D , B  = DELETE                        
002700*                                  R , Ä  = REPLACE                       
002800*                                  I,N,A  = INSERT                        
002900*                                  S , V  = SELECT                        
003000*                                  P , P  = PRINT                         
003100*                                  C , K  = COPY                          
003200        05 RESP-IDDISTR      PIC Z(5).                                    
003300*                                 DISTRIKTNUMMER                          
003400        05 RESP-IDKUNDNR     PIC Z(7).                                    
003500*                                 KUNDNUMMER                              
003600        05 RESP-IDFAKT       PIC Z(6)9.                                   
003700*                                 FAKTURANUMMER                           
003800        05 RESP-IDBYTRAP     PIC Z(6)9.                                   
003900*                                 RAPPORTNUMMER  BYTES                    
004000        05 RESP-KVRETUR-TOT  PIC -(6)9.                                   
004100*                                 ANTAL RETURER TOTALT                    
004200        05 RESP-TIREGDAT     PIC 9(6).                                    
004300*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
004400*** END OF VILMAII-COPY LENGTH= 120045 BYTES                              
