000100 01  SEQA-W6H7A1.                                                         
000200*                                 KVALITET                                
000300*                                 SEKUNDÄRT INDEX TILL W6H701             
000400*                                 KONTROLLRAPPORT                         
000500*                                 FYSISK NYCKEL: W6H7A1KY                 
000600*                                 (IDDC, DAREGDAT, KDKRSTA                
000700*                                  IDLOPNRM, IDFTG,    IDKR)              
000800*                                 SECONDARY NYCKEL: W6H7ASEQ              
000900*                                 (IDDC, DAREGDAT, KDKRSTA                
001000*                                  IDLOPNRM, IDFTG)                       
001100     03 SEQA-IDDC            PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300*                                 WAREHOUSE IDENTIFIER                    
001400     03 SEQA-DAREGDAT-9KOMPL PIC 9(8).                                    
001500*                                 DATUMETS 9-KOMPLEMENT                   
001600*                                 DATES 9-COMPLEMENT                      
001700     03 SEQA-KDKRSTA         PIC X.                                       
001800*                                 KONTROLLRAPPORT STATUS                  
001900*                                 INSPECTION REPORT STATUS                
002000     03 SEQA-IDLOPNRM        PIC S9(9)           COMP-3.                  
002100*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
002200*                                 (0VVDLLLLK)                             
002300*                                 SERIAL NO RECEIVING REPORT              
002400*                                 (0WWDLLLLC)                             
002500     03 SEQA-IDFTG           PIC 9(2).                                    
002600*                                 FÖRETAGSID EKONOM REDOVISNING           
002700*                                 COMPANY IDENTITY ACCOUNTING             
002800     03 SEQA-IDKR            PIC 9(5).                                    
002900*                                 KONTROLLRAPPORT NUMMER                  
003000*                                 INSPECTION REPORT NUMBER                
003100*** END OF VILMAII-COPY LENGTH= 23 BYTES                                  
