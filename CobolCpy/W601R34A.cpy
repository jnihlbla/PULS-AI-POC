000100 01  W601R34A-CTX.                                                        
000200*                                 LOGGPOST FÖR ATT LOGGA USER-ID          
000300*                                 DÅ R34-TRANSAR SKAPAS                   
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 IDDC                 PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800     03 IDLEVNR              PIC X(5).                                    
000900*                                 LEVERANTÖRNUMMER                        
001000     03 KDRT                 PIC S9(3)           COMP-3.                  
001100*                                 REDOVISNINGSTYP                         
001200     03 IDAVINR              PIC S9(7)           COMP-3.                  
001300*                                 AVI-NUMMER                              
001400     03 TIAVIDAT             PIC S9(7)           COMP-3.                  
001500*                                 AVISERINGSDATUM (YYMMDD)                
001600     03 IDARTNR              PIC S9(9)           COMP-3.                  
001700*                                 ARTIKELNUMMER                           
001800     03 KVAVIS               PIC S9(7)           COMP-3.                  
001900*                                 AVISERAT ANTAL                          
002000     03 IDKONTO              PIC S9(11)          COMP-3.                  
002100*                                 KONTO                                   
002200     03 IDARTNR-FROM         PIC S9(9)           COMP-3.                  
002300*                                 ARTIKELNUMMER                           
002400     03 SIGNON-USERID        PIC X(8).                                    
002500*** END OF VILMAII-COPY LENGTH= 48 BYTES                                  
