000100 01  W41831A.                                                             
000200*                                 KREDITPOST-HUVUD TILL W41831            
000300*                                 OCH BILLIT                              
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 IDLEVANM.                                                         
000700*                                 LEVERANSANMÄRKNINGSIDENTITET            
000800        05 IDDISTR           PIC S9(5)           COMP-3.                  
000900*                                 DISTRIKTNUMMER                          
001000        05 IDKUNDNR          PIC S9(7)           COMP-3.                  
001100*                                 KUNDNUMMER                              
001200        05 IDRAPPNR          PIC 9(7).                                    
001300*                                 RAPPORT NUMMER                          
001400     03 IDDC                 PIC X(2).                                    
001500*                                 IDENTIFIERARE LAGER                     
001600     03 KDLEVANM-UPD         PIC X.                                       
001700*                                 STATUS LEVERANSANMÄRKNING               
001800     03 KDARBTYP             PIC X(8).                                    
001900*                                 TYP AV ARBETE                           
002000     03 KVRADER-RT           PIC S9(5)           COMP-3.                  
002100*                                 ANTAL RADER RETURTILLSTÅND              
002200     03 IDPERSON             PIC S9(3)           COMP-3.                  
002300*                                 PERSONKOD                               
002400     03 FLFARLIG             PIC X.                                       
002500*                                 FARLIGT GODS-FLAGGA                     
002600*** END OF VILMAII-COPY LENGTH= 34 BYTES                                  
