000100 01  MID-W6I11D01.                                                        
000200*                                 MIDCOPYTEXT TILL W6011D                 
000300     03 MID-IDPGM            PIC X(8).                                    
000400*                                 PROGRAM IDENTITET                       
000500*                                 PROGRAM INTENTITY                       
000600     03 MID-KVPOST           PIC 9(7).                                    
000700*                                 POST ELLER RADRÄKNARE                   
000800*                                 RECORD OR LINE COUNTER                  
000900     03 MID-R40POST          OCCURS 22 TIMES.                             
001000        05 MID-IDLEVNR       PIC X(5).                                    
001100*                                 LEVERANTÖRNUMMER                        
001200*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001300        05 MID-IDORDNR       PIC 9(7).                                    
001400*                                 ORDERNR             IDORDNR-002         
001500*                                 ORDER NUMBER        IDORDNR-002         
001600        05 MID-IDARTNR       PIC 9(8).                                    
001700*                                 ARTIKELNUMMER                           
001800*                                 PART NUMBER                             
001900        05 MID-KVANTAL       PIC S9(7).                                   
002000*                                 ANTAL                                   
002100*                                 NUMBER                                  
002200        05 MID-FLUPPBR       PIC 9.                                       
002300*                                 BESTÄLLNINGSREST UPPDATERAS?            
002400*                                 (1 = JA)                                
002500        05 MID-IDDC          PIC X(2).                                    
002600*                                 IDENTIFIERARE LAGER                     
002700*                                 WAREHOUSE IDENTIFIER                    
002800     03 MID-KVART-SKROT-LDC  PIC 9(7).                                    
002900*                                 ANTAL ARTNR PER BRYTBEGREPP             
003000*                                 NO OF PARTNOS PER TYPE                  
003100*** END OF VILMAII-COPY LENGTH= 682 BYTES                                 
