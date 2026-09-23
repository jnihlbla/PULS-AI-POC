000100 01  WDD905DL-CTX.                                                        
000200*                                 DELIVERY PLAN REGISTER                  
000300*                                 CALLOFF INFORMATION                     
000400     03 IDSEGM               PIC X(6).                                    
000500*                                 SEGMENT                                 
000600     03 FILLERX2             PIC X(2).                                    
000700     03 IDARTNR-KEY          PIC S9(9)           COMP-3.                  
000800*                                 ARTIKELNUMMER                           
000900     03 IDDC-KEY             PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 IDLEVNR-KEY          PIC X(5).                                    
001200*                                 LEVERANTÖRNUMMER                        
001300     03 DAAVROP-KEY          PIC 9(6).                                    
001400*                                 AVROPSVECKA   (ÅÅÅÅVV)                  
001500     03 TILEVDAG-KEY         PIC S9              COMP-3.                  
001600*                                 AVSÄNDNINGSDAG INOM VECKA               
001700     03 FILLERX19            PIC X(19).                                   
001800     03 WDD905-CTX.                                                       
001900*                                 LEVERANSPLANEREGISTER                   
002000*                                 AVROPSINFORMATION                       
002100*                                 FYSISK NYCKEL: WDD905KY                 
002200*                                 (DAAVROP(-AVS), TILEVDAG)               
002300*                                 SÖKARGUMENT    KDAVROP                  
002400        05 KDAVROP           PIC S9              COMP-3.                  
002500*                                 AVROPSKOD                               
002600        05 DAAVROP-AVS       PIC 9(6).                                    
002700*                                 AVSÄNDNINGSVECKA (PLANERAD)             
002800*                                 (ÅÅÅÅVV)                                
002900        05 TILEVDAG          PIC S9              COMP-3.                  
003000*                                 AVSÄNDNINGSDAG INOM VECKA               
003100        05 TIAVRDAT-INL      PIC S9(7)           COMP-3.                  
003200*                                 PLANERAT INLEVERANSDATUM                
003300        05 TIAVRDAT-DISP     PIC S9(7)           COMP-3.                  
003400*                                 PLANERAT DISPONIBLEDATUM                
003500        05 KVAVROP           PIC S9(7)           COMP-3.                  
003600*                                 AVROPSKVANTITET                         
003700*** END OF VILMAII-COPY LENGTH= 66 BYTES                                  
