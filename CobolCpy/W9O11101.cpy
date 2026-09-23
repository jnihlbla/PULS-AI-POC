000100 01  MOD-W9O11101.                                                        
000200*                                 COPYTEXT FÖR MOD                        
000300*                                 W9O11101                                
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-MESSAGE-RAD1     PIC X(40).                                   
000700*                                 MEDDELANDEFÄLT PÅ RAD 1                 
000800     03 MOD-IDARTNR-IN       PIC X(2).                                    
000900*                                 MFS BEHANDLING AV INPUTFÄLT             
001000     03 MOD-IDARTNR-UT       PIC X(9).                                    
001100*                                 ARTIKELNUMMER                           
001200     03 MOD-BEART-SVE        PIC X(25).                                   
001300*                                 SVENSK ARTIKELBENÄMNING                 
001400     03 MOD-BEART-ENG        PIC X(25).                                   
001500*                                 ENGELSK ARTIKELBENÄMNING                
001600     03 MOD-IDANSK           PIC Z(2)9.                                   
001700*                                 ANSKAFFARNUMMER                         
001800     03 MOD-PRARTSJK         PIC Z(6)9.9(2).                              
001900*                                 ARTIKELNS SJÄLVKOSTNAD                  
002000     03 MOD-IDINK            PIC X(4).                                    
002100*                                 INKÖPARNUMMER                           
002200     03 MOD-KVDISP           PIC Z(5)9.                                   
002300*                                 DISPONIBELT LAGER                       
002400     03 MOD-IDLEVNR          PIC X(5).                                    
002500*                                 LEVERANTÖRNUMMER                        
002600     03 MOD-KVPB-TOT         PIC Z(5)9.9.                                 
002700*                                 TOTALT PERIODBEHOV                      
002800     03 MOD-KDHF             PIC 9.                                       
002900*                                 HUVUDFÖRRÅDSMÄRKNING                    
003000     03 MOD-KDIART           PIC X.                                       
003100*                                 INGÅR I SATS                            
003200     03 MOD-KDSRA            PIC Z9.                                      
003300*                                 SRA-KOD                                 
003400     03 MOD-KDARTURS         PIC X(2).                                    
003500*                                 ARTIKELURSPRUNGSKOD                     
003600     03 MOD-KDTULLRE-ATTR    PIC X(2).                                    
003700*                                 MFS ATTRIBUTFÄLT                        
003800     03 MOD-KDTULLRE         PIC 9.                                       
003900*                                 TULLRESTITUTION MÄRKNING                
004000     03 MOD-KDTULLRE-NY-ATTR PIC X(2).                                    
004100*                                 MFS ATTRIBUTFÄLT                        
004200     03 MOD-KDTULLRE-NY      PIC X(2).                                    
004300*                                 MFS BEHANDLING AV INPUTFÄLT             
004400     03 MOD-MESSAGE-RAD23    PIC X(79).                                   
004500*                                 MEDDELANDEFÄLT PÅ RAD 23                
004600*** END OF VILMAII-COPY LENGTH= 233 BYTES                                 
