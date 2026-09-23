000100 01  W2712D.                                                              
000200*                                 ARTIKLAR F÷R AUT. GODK. SKROT           
000300*                                 PASSIVERADE ART. SDC OCH NDC            
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 IDARTNR              PIC S9(9)           COMP-3.                  
000700*                                 ARTIKELNUMMER                           
000800     03 IDDC                 PIC X(2).                                    
000900*                                 IDENTIFIERARE LAGER                     
001000     03 DADATUM              PIC 9(8).                                    
001100*                                 REGISTRERINGSDATUM (≈≈≈≈MMDD)           
001200     03 IDDISTR              PIC S9(5)           COMP-3.                  
001300*                                 DISTRIKTNUMMER                          
001400     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
001500*                                 KUNDNUMMER                              
001600     03 SUARTSTD             PIC S9(9)V9(2)      COMP-3.                  
001700*                                 SUMMA STANDARDPRIS RADVƒRDE             
001800     03 KVSKROT              PIC S9(7)           COMP-3.                  
001900*                                 ANTAL SENASTE SKROTORDER                
002000     03 KDDC                 PIC X(2).                                    
002100*                                 TYP AV DISTR. LAGER                     
002200*** END OF VILMAII-COPY LENGTH= 37 BYTES                                  
