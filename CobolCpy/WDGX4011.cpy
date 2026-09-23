000100 01  4011-WDGX4011.                                                       
000200*                                 UTSKRIFT AV PLOCKSATSER                 
000300*                                 TEMPORÄR LAGRING MEDAN                  
000400*                                 BLÄDDRING (PF8) SKER                    
000500*                                 FYSISK NYCKEL WDGXKEY:                  
000600*                                 (IDHTYP   + IDDC +                      
000700*                                  IDUSER   + LOWVALUE)                   
000800     03 4011-IDHTYP          PIC X(4).                                    
000900*                                 HÄNDELSETYP                             
001000     03 4011-IDDC            PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200*                                 WAREHOUSE IDENTIFIER                    
001300     03 4011-IDUSER          PIC X(8).                                    
001400*                                 ANVÄNDARENS SÄKERHETS ID                
001500*                                 USER SECURITY-IDENTITY                  
001600     03 4011-LOWVALUE        PIC X(16).                                   
001700*** END OF VILMAII-COPY LENGTH= 30 BYTES                                  
