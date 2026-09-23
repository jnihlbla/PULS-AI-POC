000100 01  2238-WDGX2238.                                                       
000200*                                 SATS                                    
000300*                                 FR≈N SATSORDER TILL ANSK                
000400*                                 FYSISK NYCKEL WDGXKEY:                  
000500*                                 IDARTNR, TIBEHOV, LOW-VALUE             
000600     03 2238-IDARTNR         PIC S9(9)           COMP-3.                  
000700*                                 ARTIKELNUMMER                           
000800*                                 PART NUMBER                             
000900     03 2238-TIBEHOV         PIC S9(5)           COMP-3.                  
001000*                                 BEHOVSVECKA           (≈≈VV)            
001100     03 2238-LOW-VALUE       PIC X(2).                                    
001200     03 2238-IDORDNSB        PIC S9(5)           COMP-3.                  
001300*                                 SATSORDERNUMMER-BAS                     
001400*                                 KIT-ORDER-NUMBER-BASIC                  
001500     03 2238-FILLER          PIC X(7).                                    
001600*** END COPY WDGX2238C0  LENGTH=20                                        
