000100 01  BINL-WDL622.                                                         
000200*                                 INLEVERANS HISTORIK - NDC               
000300*                                 BINNING LIST INFO                       
000400*                                 FYSISK NYCKEL: KDSEGKEY                 
000500     03 BINL-KDSEGKEY        PIC X.                                       
000600*                                 TEKNISK SEGMENT-NYCKEL                  
000700     03 BINL-IDILIST         PIC 9(5).                                    
000800*                                 INLÄGGNINGSLISTEIDENTITET               
000900     03 BINL-IDDC            PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 BINL-ADART.                                                       
001200*                                 ARTIKELADRESS I LAGRET                  
001300        05 BINL-ADLAGOMR     PIC S9(3)           COMP-3.                  
001400*                                 LAGEROMRÅDE                             
001500        05 BINL-ADGANG       PIC S9(3)           COMP-3.                  
001600*                                 GÅNG                                    
001700        05 BINL-ADPLATS      PIC S9(5)           COMP-3.                  
001800*                                 LAGERPLATSNUMMER                        
001900     03 BINL-ADINLOMR-ILI    PIC X(4).                                    
002000*                                 PLACERING INLÄGGNINGS LISTA             
002100     03 BINL-KVANTAL-ILI     PIC S9(7)           COMP-3.                  
002200*                                 ANTAL PÅ INLÄGGNINGSLISTA               
002300*                                                                         
002400     03 BINL-TIUPPDAT-ILI    PIC S9(7)           COMP-3.                  
002500*                                 UPPD.DATUM PÅ INLÄGGNINGSLISTA          
002600     03 BINL-IDANSTNR-ILIU   PIC X(5).                                    
002700*                                 ANSTÄLLNINGSNUMMER I-LIST UPPD          
002800*** END OF VILMAII-COPY LENGTH= 32 BYTES                                  
