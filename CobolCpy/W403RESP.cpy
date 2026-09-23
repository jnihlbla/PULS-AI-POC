000100 01  RESP-W403RESP.                                                       
000200*                                 3IV-MEDDELANDE RESPONSE HEADER          
000300*                                 3IV MESSAGE RESPONSE HEADER             
000400     03 RESP-IDMSG3IV        PIC X(30).                                   
000500*                                 3IV MEDDELANDE-ID                       
000600*                                 3IV MESSAGE ID                          
000700     03 RESP-IDMTYP3IV       PIC X(30).                                   
000800*                                 3IV MEDDELANDE-TYP                      
000900*                                 3IV MESSAGE TYPE                        
001000     03 RESP-TISTAMP3IV      PIC X(17).                                   
001100*                                 TIDPUNKT F÷R 3IV-MEDDELANDE             
001200*                                 (≈≈≈≈MMDDHHMMSSTTT)                     
001300*                                 TIMESTAMP FOR 3IV MESSAGE               
001400*                                 (YYYYMMDDHHMMSSTTT)                     
001500     03 RESP-IDDC            PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700*                                 WAREHOUSE IDENTIFIER                    
001800     03 RESP-IDANSTNR        PIC Z(4)9.                                   
001900*                                 ANSTƒLLNINGSNUMMER                      
002000*                                 IDENTIFICATION NO EMPLOYEE              
002100     03 RESP-IDSNO3IV        PIC X(25).                                   
002200*                                 SERIENR P≈ TERMINAL I 3IV               
002300*                                 SERIAL NO OF TERMINAL IN 3IV            
002400     03 RESP-KDRESP3IV       PIC Z(2)9.                                   
002500*                                 SVARSKOD. 0=OK, NNN=FEL                 
002600*                                 RESPONSE CODE. 0=OK, NNN=ERROR          
002700     03 RESP-BERESP3IV       PIC X(50).                                   
002800*                                 MEDDELANDETEXT                          
002900*                                                                         
003000*                                 MESSAGE TEXT                            
003100*                                                                         
003200*** END OF VILMAII-COPY LENGTH= 162 BYTES                                 
