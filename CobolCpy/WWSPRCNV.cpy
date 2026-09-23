000010*** EDIT ALLOWED                                                          
000100 01  WWSPRCNV.                                                            
000200*                                 ANVÄNDS VID KONVERTERING MELLAN         
000300*                                 ISO839 SPRÅKKOD OCH                     
000310*                                 KATALOGSYSTEMETS SPRÅKSTANDARD          
000400*                                 UTTRYCKT I IDSKYLT                      
000420*                                                                         
000500     03 WWSPRCNV-MAX-ANTAL   PIC S9(9)           COMP                     
000600                             VALUE +16.                                   
000700     03 WWSPRCNV-TABELL.                                                  
000701*                                 TYSKA                                   
000710        05 FILLER  PIC X(3)  VALUE 'D  '.                                 
000711        05 FILLER  PIC X(2)  VALUE 'DE'.                                  
000712*                                 SPANSKA                                 
000713        05 FILLER  PIC X(3)  VALUE 'E  '.                                 
000714        05 FILLER  PIC X(2)  VALUE 'ES'.                                  
000715*                                 FRANSKA                                 
000716        05 FILLER  PIC X(3)  VALUE 'F  '.                                 
000717        05 FILLER  PIC X(2)  VALUE 'FR'.                                  
000718*                                 ENGELSKA                                
000719        05 FILLER  PIC X(3)  VALUE 'GB '.                                 
000720        05 FILLER  PIC X(2)  VALUE 'EN'.                                  
000721*                                 ITALIENSKA                              
000722        05 FILLER  PIC X(3)  VALUE 'I  '.                                 
000723        05 FILLER  PIC X(2)  VALUE 'IT '.                                 
000724*                                 HOLLÄNDSKA                              
000725        05 FILLER  PIC X(3)  VALUE 'NL '.                                 
000726        05 FILLER  PIC X(2)  VALUE 'NL'.                                  
000727*                                 PORTUGISISKA                            
000728        05 FILLER  PIC X(3)  VALUE 'P  '.                                 
000729        05 FILLER  PIC X(2)  VALUE 'PT'.                                  
000730*                                 SVENSKA                                 
000731        05 FILLER  PIC X(3)  VALUE 'S  '.                                 
000732        05 FILLER  PIC X(2)  VALUE 'SV'.                                  
000733*                                 FINSKA                                  
000734        05 FILLER  PIC X(3)  VALUE 'SF '.                                 
000735        05 FILLER  PIC X(2)  VALUE 'FI'.                                  
000736*                                 AMERIKANSKA                             
000737        05 FILLER  PIC X(3)  VALUE 'USA'.                                 
000740        05 FILLER  PIC X(2)  VALUE 'US'.                                  
000750*                                 JAPANSKA                                
000800        05 FILLER  PIC X(3)  VALUE 'J  '.                                 
000900        05 FILLER  PIC X(2)  VALUE 'JA'.                                  
000910*                                 KOREANSKA                               
001000        05 FILLER  PIC X(3)  VALUE 'KOR'.                                 
001100        05 FILLER  PIC X(2)  VALUE 'KO'.                                  
001110*                                 MALAY (BAHASA)                          
001200        05 FILLER  PIC X(3)  VALUE 'MAL'.                                 
001300        05 FILLER  PIC X(2)  VALUE 'ML'.                                  
001310*                                 KINESISKA                               
001400        05 FILLER  PIC X(3)  VALUE 'RC '.                                 
001500        05 FILLER  PIC X(2)  VALUE 'ZH'.                                  
001510*                                 RYSKA                                   
001600        05 FILLER  PIC X(3)  VALUE 'RUS'.                                 
001700        05 FILLER  PIC X(2)  VALUE 'RU'.                                  
001710*                                 THAI                                    
001800        05 FILLER  PIC X(3)  VALUE 'T  '.                                 
001810        05 FILLER  PIC X(2)  VALUE 'TH'.                                  
001900                                                                          
002800     03 FILLER REDEFINES WWSPRCNV-TABELL.                                 
002900        05 WWSPRCNV-RAD      OCCURS 16 TIMES                              
003100                             INDEXED WWSPRCNV-IX.                         
003200           07 WWSPRCNV-SPRAK-IDSKYLT  PIC X(3).                           
003210*                                 NATIONALITETSTECKEN                     
003300           07 WWSPRCNV-KDSPRAK        PIC X(2).                           
003400*                                 2-STÄLLIG ISO839 SPRÅKKOD               
003410*                                                                         
003500*** END COPY WWSPRCNV    LENGTH=84                                        
