000100 01  REQU-W60139I1.                                                       
000200*                                 COPYTEXT FÖR REQU                       
000300*                                 W60139I1                                
000400     03 REQU-IDLEVNR-KOLLI   PIC X(5).                                    
000500*                                 LEVERANTÖRNUMMER KOLLI                  
000600     03 REQU-IDOKOLLI        PIC X(9).                                    
000700*                                 ODETTE KOLLINUMMER                      
000800     03 REQU-IDLOPNRM-KEY    PIC X(9).                                    
000900*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001000*                                 (0VVDLLLLK)                             
001100     03 REQU-IDDC-KEY        PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300     03 REQU-IDKR-START      PIC 9(5).                                    
001400*                                 KONTROLLRAPPORT NUMMER                  
001500     03 REQU-IDKVAINF-START  PIC 9(2).                                    
001600*                                 RADNR FÖR KVALITETSKONTROLLTEXT         
001700     03 REQU-FLAGGA-KR-HOPP  PIC X.                                       
001800*                                 GOKDKÄND                                
001900     03 REQU-INPUT.                                                       
002000        05 REQU-IDUSER-PRI   PIC X(8).                                    
002100*                                 ANVÄNDARENS SÄKERHETS ID                
002200        05 REQU-FLAGGA-PRI   PIC X.                                       
002300*                                 GOKDKÄND                                
002400        05 REQU-BEANST       PIC X(25).                                   
002500*                                 ANSTÄLLDS NAMN                          
002600        05 REQU-IDUSER-SEK   PIC X(8).                                    
002700*                                 ANVÄNDARENS SÄKERHETS ID                
002800        05 REQU-FLAGGA-SEK   PIC X.                                       
002900*                                 GOKDKÄND                                
003000        05 REQU-IDUSER-ADM   PIC X(8).                                    
003100*                                 ANVÄNDARENS SÄKERHETS ID                
003200        05 REQU-FLAGGA-ADM   PIC X.                                       
003300*                                 GOKDKÄND                                
003400        05 REQU-FLAGGA-GODK  PIC X.                                       
003500*                                 GOKDKÄND                                
003600        05 REQU-IDUSER-APR   PIC X(8).                                    
003700*                                 ANVÄNDARENS SÄKERHETS ID                
003800     03 REQU-IDSPRAK         PIC X(2).                                    
003900*                                 2-STÄLLIG ISO SPRÅKKOD                  
004000*** END OF VILMAII-COPY LENGTH= 96 BYTES                                  
