000100 01  WDD924DL-CTX.                                                        
000200*                                 DELIVERY PLAN REGISTER                  
000300*                                 DELIVERY INFORMATION                    
000400     03 IDSEGM               PIC X(6).                                    
000500*                                 SEGMENT                                 
000600     03 FILLERX2             PIC X(2).                                    
000700     03 IDARTNR-KEY          PIC S9(9)           COMP-3.                  
000800*                                 ARTIKELNUMMER                           
000900     03 IDDC-KEY             PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 IDLEVNR-KEY          PIC X(5).                                    
001200*                                 LEVERANT÷RNUMMER                        
001300     03 DALEVBSK-AVS-KEY     PIC 9(8).                                    
001400*                                 LEV. BESK. AVS. DAT.(≈≈≈≈MMDD)          
001500     03 FILLERX18            PIC X(18).                                   
001600     03 WDD924-CTX.                                                       
001700*                                 LEVERANSPLANEREGISTER                   
001800*                                 LEVERANSBESKED                          
001900*                                 FYSISK NYCKEL: DALEVBSK(-AVS)           
002000        05 DALEVBSK-AVS      PIC 9(8).                                    
002100*                                 LEV. BESK. AVS. DAT.(≈≈≈≈MMDD)          
002200        05 TILEVBSK-INL      PIC S9(7)           COMP-3.                  
002300*                                 LEV. BESK. INLEV. DAT(≈≈MMDD)           
002400        05 TILEVBSK-DISP     PIC S9(7)           COMP-3.                  
002500*                                 LEV. BESK. DISPONIBEL(≈≈MMDD)           
002600        05 KVAVIS-BSKURS     PIC S9(7)           COMP-3.                  
002700*                                 URSPRUNGL LEVERANSBESKEDSANTAL          
002800        05 KVAVIS-BSKKVAR    PIC S9(7)           COMP-3.                  
002900*                                 LEV. BESK. ANT. EFTER AVBOKNING         
003000        05 FLFORAVI          PIC X.                                       
003100*                                 F÷RAVISERAD INLEVERANS                  
003200        05 FLSENLEV          PIC X.                                       
003300*                                 F÷RSENAD LEVERANS                       
003400        05 TIREGDAT          PIC S9(7)           COMP-3.                  
003500*                                 REGISTRERINGSDATUM (≈≈MMDD)             
003600        05 TIREGTID          PIC S9(7)           COMP-3.                  
003700*                                 REGISTRERINGSTID                        
003800*** END OF VILMAII-COPY LENGTH= 80 BYTES                                  
