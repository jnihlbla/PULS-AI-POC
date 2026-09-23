000100 01  TAB-WDN111.                                                          
000200*                                 KATALOGREGISTER                         
000300*                                 VADIS GENERERINGSTABELL                 
000400*                                 FYSISK NYCKEL: TIAAAA                   
000500     03 TAB-TIAAAA           PIC 9(4).                                    
000600*                                 ≈RTAL (≈≈≈≈)                            
000700*                                 YEAR  (YYYY)                            
000800     03 TAB-PERIOD           OCCURS 12 TIMES.                             
000900*                                 PERIOD OMBRYTNING                       
001000        05 TAB-IDUSER        PIC X(8).                                    
001100*                                 ANVƒNDARENS SƒKERHETS ID                
001200*                                 USER SECURITY-IDENTITY                  
001300        05 TAB-FLVADGEN      PIC X.                                       
001400*                                 AKTIVERA VADIS-GENERERING               
001500*                                 VADIS GENERATE FLAG                     
001600        05 TAB-KDCATPUB-FOM  PIC X(6).                                    
001700*                                 PUBLICERINGS TIDKOD, F.O.M.             
001800*                                 RELEASE TIME CODE, FROM                 
001900        05 TAB-KDCATPUB-TOM  PIC X(6).                                    
002000*                                 PUBLICERINGS TIDKOD, T.O.M.             
002100*                                 RELEASE TIME CODE, TO                   
002200        05 TAB-TIOMBRYT      PIC S9(7)           COMP-3.                  
002300*                                 OMBRYTNINGSDATUM                        
002400*                                 DATE OF PAGE MAKING UP                  
002500        05 TAB-TIUPPDAT      PIC S9(7)           COMP-3.                  
002600*                                 UPPDATERINGSDATUM  (≈≈MMDD)             
002700*                                 UPDATING DATE     (YYMMDD)              
002800        05 TAB-TIVADGEN-PLAN PIC S9(7)           COMP-3.                  
002900*                                 PLANERAT K÷RDATUM (≈≈MMDD)              
003000*                                 PLANNED EXECUTION DATE (YYMMDD)         
003100        05 TAB-TIVADGEN-UPPD PIC S9(7)           COMP-3.                  
003200*                                 VADIS UPPDATERAT (≈≈MMDD)               
003300*                                 VADIS UPDATED   (YYMMDD)                
003400*** END OF VILMAII-COPY LENGTH= 448 BYTES                                 
