000100 01  MID-W5I30401.                                                        
000200*                                 MID-COPYTEXT FÖR BILD                   
000300*                                 INVENTERING RE1                         
000400     03 MID-IDDC-IN          PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600     03 MID-IDDC-UT          PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800     03 MID-INV-RE1-GRP      OCCURS 24 TIMES.                             
000900*                                 INVENTERINGSARTIKELGRUPP                
001000        05 MID-IDARTNR       PIC X(9).                                    
001100*                                 ARTIKELNUMMER                           
001200        05 MID-KDINVPRIO     PIC 9.                                       
001300*                                 INVENTERING PRIORITET                   
001400        05 MID-TEINVANM      PIC X(25).                                   
001500*                                 INVENTERINGSANMÄRKNING                  
