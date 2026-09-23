000100 01  MID-W4I37A01.                                                        
000200*                                 MID-COPYTEXT FÖR PGM   437A             
000300*                                 SEND RESPONSE CUSTOMER SUMMARY          
000400     03 MID-IDPRODNR         PIC X(7).                                    
000500*                                 PRODUKTIONSNUMMER                       
000600     03 MID-IDPLKLST         PIC X(3).                                    
000700*                                 PLOCKLISTNUMMER                         
000800     03 MID-IDPRC.                                                        
000900*                                 PRODUKTIONSKANAL                        
001000        05 MID-IDPRCBAS      PIC X(3).                                    
001100*                                 PRC-BAS                                 
001200        05 MID-IDPRCVAR      PIC X.                                       
001300*                                 PRC-VARIANT                             
001400     03 MID-IDLOPNR          PIC X(3).                                    
001500*                                 LÖPNUMMER                               
001600     03 MID-IDDC             PIC X(2).                                    
001700*                                 IDENTIFIERARE LAGER                     
001800     03 MID-IDUSER           PIC X(8).                                    
001900*                                 ANVÄNDARENS SÄKERHETS ID                
002000*** END OF VILMAII-COPY LENGTH= 27 BYTES                                  
