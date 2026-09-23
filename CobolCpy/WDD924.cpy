000100 01  LEV-WDD924.                                                          
000200*                                 LEVERANSPLANEREGISTER                   
000300*                                 LEVERANSBESKED                          
000400*                                 FYSISK NYCKEL: DALEVBSK(-AVS)           
000500     03 LEV-DALEVBSK-AVS     PIC 9(8).                                    
000600*                                 LEV. BESK. AVS. DAT.(≈≈≈≈MMDD)          
000700*                                 DELIVERY DATE FROM SUPPLIER             
000800     03 LEV-TILEVBSK-INL     PIC S9(7)           COMP-3.                  
000900*                                 LEV. BESK. INLEV. DAT(≈≈MMDD)           
001000*                                 DELIVERY DATE TO WAREHOUSE              
001100     03 LEV-TILEVBSK-DISP    PIC S9(7)           COMP-3.                  
001200*                                 LEV. BESK. DISPONIBEL(≈≈MMDD)           
001300*                                 DELIVERY DATE IN STOCK                  
001400     03 LEV-KVAVIS-BSKURS    PIC S9(7)           COMP-3.                  
001500*                                 URSPRUNGL LEVERANSBESKEDSANTAL          
001600*                                 ORIGINAL DELIVERY QUANTITY              
001700     03 LEV-KVAVIS-BSKKVAR   PIC S9(7)           COMP-3.                  
001800*                                 LEV. BESK. ANT. EFTER AVBOKNING         
001900*                                 DELIV QUANT AFTER STOCK ALLOC           
002000     03 LEV-FLFORAVI         PIC X.                                       
002100*                                 F÷RAVISERAD INLEVERANS                  
002200*                                 PREADVICE DELIVERY                      
002300     03 LEV-FLSENLEV         PIC X.                                       
002400*                                 F÷RSENAD LEVERANS                       
002500*                                 DELAIED DELIVERY FLAG                   
002600     03 LEV-TIREGDAT         PIC S9(7)           COMP-3.                  
002700*                                 REGISTRERINGSDATUM (≈≈MMDD)             
002800*                                 REGISTRATION DATE (YYMMDD)              
002900     03 LEV-TIREGTID         PIC S9(7)           COMP-3.                  
003000*                                 REGISTRERINGSTID                        
003100*                                 GENERAL REGISTRATION TIME               
003200*** END OF VILMAII-COPY LENGTH= 34 BYTES                                  
