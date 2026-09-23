000100 01  REQU-W403REQU.                                                       
000200*                                 3IV-MEDDELANDE REQUEST HEADER           
000300*                                 3IV MESSAGE REQUEST HEADER              
000400     03 REQU-IDMSG3IV        PIC X(30).                                   
000500*                                 3IV MEDDELANDE-ID                       
000600*                                 3IV MESSAGE ID                          
000700     03 REQU-IDMVER3IV       PIC X(10).                                   
000800*                                 3IV MEDDELANDE-VERSION                  
000900*                                 3IV MESSAGE VERSION                     
001000     03 REQU-IDMTYP3IV       PIC X(30).                                   
001100*                                 3IV MEDDELANDE-TYP                      
001200*                                 3IV MESSAGE TYPE                        
001300     03 REQU-TISTAMP3IV      PIC X(17).                                   
001400*                                 TIDPUNKT F÷R 3IV-MEDDELANDE             
001500*                                 (≈≈≈≈MMDDHHMMSSTTT)                     
001600*                                 TIMESTAMP FOR 3IV MESSAGE               
001700*                                 (YYYYMMDDHHMMSSTTT)                     
001800     03 REQU-IDDC            PIC X(2).                                    
001900*                                 IDENTIFIERARE LAGER                     
002000*                                 WAREHOUSE IDENTIFIER                    
002100     03 REQU-IDANSTNR        PIC 9(5).                                    
002200*                                 ANSTƒLLNINGSNUMMER                      
002300*                                 IDENTIFICATION NO EMPLOYEE              
002400     03 REQU-IDSNO3IV        PIC X(25).                                   
002500*                                 SERIENR P≈ TERMINAL I 3IV               
002600*                                 SERIAL NO OF TERMINAL IN 3IV            
002700*** END OF VILMAII-COPY LENGTH= 119 BYTES                                 
