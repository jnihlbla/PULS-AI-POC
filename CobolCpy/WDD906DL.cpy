000100 01  WDD906DL-CTX.                                                        
000200*                                 DELIVERY PLAN REGISTER                  
000300*                                 CALLOFF RECEIVED                        
000400*                                                                         
000500     03 IDSEGM               PIC X(6).                                    
000600*                                 SEGMENT                                 
000700     03 FILLERX2             PIC X(2).                                    
000800     03 IDARTNR-KEY          PIC S9(9)           COMP-3.                  
000900*                                 ARTIKELNUMMER                           
001000     03 IDDC-KEY             PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200     03 IDLEVNR-KEY          PIC X(5).                                    
001300*                                 LEVERANT÷RNUMMER                        
001400     03 DAAVROP-KEY          PIC 9(6).                                    
001500*                                 AVROPSVECKA   (≈≈≈≈VV)                  
001600     03 TILEVDAG-KEY         PIC S9              COMP-3.                  
001700*                                 AVSƒNDNINGSDAG INOM VECKA               
001800     03 IDLOPNRM-PL-KEY      PIC S9(9)           COMP-3.                  
001900*                                 AVBOKNINGSID, (≈≈VVDLLLL)               
002000     03 FILLERX14            PIC X(14).                                   
002100     03 WDD906-CTX.                                                       
002200*                                 LEVERANSPLANEREGISTER                   
002300*                                 AVBOKNINGSINFO-AVROP                    
002400*                                 FYSISK NYCKEL  IDLOPNRM(-PL)            
002500        05 IDLOPNRM-PL       PIC S9(9)           COMP-3.                  
002600*                                 AVBOKNINGSID, (≈≈VVDLLLL)               
002700        05 KVAVROP-AVB       PIC S9(7)           COMP-3.                  
002800*                                 AVBOKAT ANTAL                           
002900*** END OF VILMAII-COPY LENGTH= 55 BYTES                                  
