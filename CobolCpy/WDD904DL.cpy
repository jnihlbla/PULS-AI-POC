000100 01  WDD904DL-CTX.                                                        
000200*                                 DELIVERY PLAN REGISTER                  
000300*                                 RESCHEDULE INFORMATION                  
000400*                                                                         
000500     03 IDSEGM               PIC X(6).                                    
000600*                                 SEGMENT                                 
000700     03 FILLERX2             PIC X(2).                                    
000800     03 IDARTNR-KEY          PIC S9(9)           COMP-3.                  
000900*                                 ARTIKELNUMMER                           
001000     03 IDDC-KEY             PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200     03 IDLEVNR-KEY          PIC X(5).                                    
001300*                                 LEVERANTÖRNUMMER                        
001400     03 FILLERX26            PIC X(26).                                   
001500     03 WDD904-CTX.                                                       
001600*                                 LEVERANSPLANEREGISTER                   
001700*                                 OMSPEC-INFORMATION(BARA 1 SEGM)         
001800*                                 SÖKFÄLT:  DASPECST                      
001900        05 DASPECST          PIC 9(6).                                    
002000*                                 SPECAD FR.O.M DATUM   (ÅÅÅÅVV)          
002100        05 KDLPORS-TAB       OCCURS 3 TIMES                               
002200                             PIC S9(3)           COMP-3.                  
002300*                                 LEVERANSPLANEORSAK                      
002400        05 KVBEST-PL         PIC S9(7)           COMP-3.                  
002500*                                 BESTÄLLNINGSKVANTITET PÅ PLAN           
002600        05 KDPLKOEP          PIC S9              COMP-3.                  
002700*                                 STATUS AVTALSKÖP (PLAN)                 
002800*                                 1=FÖRESLAGEN  2=GODKÄND                 
002900*** END OF VILMAII-COPY LENGTH= 63 BYTES                                  
