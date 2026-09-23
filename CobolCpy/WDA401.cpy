000100 01  BUYB-WDA401.                                                         
000200*                                 BUY BACK RETURER                        
000300*                                 FYSISK NYCKEL: WDA401KY:                
000400*                                 (IDDISTR + IDRAPPNR)                    
000500     03 BUYB-IDDISTR         PIC S9(5)           COMP-3.                  
000600*                                 DISTRIKTNUMMER                          
000700*                                 DISTRICT NUMBER                         
000800     03 BUYB-IDRAPPNR        PIC 9(7).                                    
000900*                                 RAPPORT NUMMER                          
001000*                                 DISCREPANCY REPORT NUMBER               
001100     03 BUYB-IDKUNDNR        PIC S9(7)           COMP-3.                  
001200*                                 KUNDNUMMER                              
001300*                                 CUSTOMER NO                             
001400     03 BUYB-FLPERMIT        PIC X.                                       
001500*                                 FLAGGA RETURTILLSTÅND                   
001600*                                 PERMITT FLAG                            
001700     03 BUYB-FLPRINT         PIC X.                                       
001800*                                 FLAGGA PRINTAD                          
001900*                                 PRINT FLAG                              
002000     03 BUYB-REFOBNET        PIC S9(3)           COMP-3.                  
002100*                                 FOBNET I PROCENT                        
002200*                                 FOBNET PERCENT                          
002300     03 BUYB-SUMINVD         PIC S9(3)           COMP-3.                  
002400*                                 MIN VÄRDE FÖR EN ORDERAD                
002500*                                 MIN VALUE FOR A ORDER ROW               
002600     03 BUYB-TIREGDAT        PIC S9(7)           COMP-3.                  
002700*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
002800*                                 REGISTRATION DATE (YYMMDD)              
002900     03 BUYB-TIREGTID        PIC S9(7)           COMP-3.                  
003000*                                 REGISTRERINGSTID                        
003100*                                 GENERAL REGISTRATION TIME               
003200     03 BUYB-FILLER          PIC X(10).                                   
003300*** END OF VILMAII-COPY LENGTH= 38 BYTES                                  
