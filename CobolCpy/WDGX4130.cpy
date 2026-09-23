000100 01  4130-WDGX4130.                                                       
000200*                                 STYRNING LEVANM. TILL REMISS            
000300*                                 DC SEGMENT                              
000400*                                 NYCKEL = IDDC                           
000500     03 4130-IDDC            PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700*                                 WAREHOUSE IDENTIFIER                    
000800     03 4130-IDUSER          PIC X(8).                                    
000900*                                 ANVÄNDARENS SÄKERHETS ID                
001000*                                 USER SECURITY-IDENTITY                  
001100     03 4130-TIUPPDAT        PIC S9(7)           COMP-3.                  
001200*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
001300*                                 UPDATING DATE     (YYMMDD)              
001400*** END OF VILMAII-COPY LENGTH= 14 BYTES                                  
