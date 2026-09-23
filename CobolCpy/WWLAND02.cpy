000010*** EDIT ALLOWED                                                          
000100 01  02-IDSKYLT-KONTROLL.                                                 
000200*      KNTRL-COPYTEXT AV IDSKYLT-VÄRDEN, SPRÅKIDENTIFIKATION              
000210*                               NATIONALITETSTECKEN BILSKYLT              
000300*                                                                         
000400     03 02-IDSKYLT           PIC X(3).                                    
000410*                            EBCDIC  EDITERBARA I 3270-MILJÖ              
000500        88 02-GODK-IDSKYLT             VALUE 'D  '                        
000600                                             'DK '                        
000610                                             'E  '                        
000700                                             'F  '                        
000800                                             'GB '                        
000900                                             'I  '                        
001000                                             'MAL'                        
001010                                             'NL '                        
001100                                             'P  '                        
001200                                             'S  '                        
001300                                             'SF '                        
001400                                             'USA'.                       
001410                                                                          
001700     03 02-IDSKYLT-UTF8      PIC X(3).                                    
001701*                                    SPRÅK LAGRADE I UNICODE              
001800        88 02-GODK-IDSKYLT-UTF8        VALUE 'CZ '                        
001810                                             'J  '                        
001820                                             'GR '                        
001830                                             'H  '                        
001840                                             'IR '                        
001900                                             'KOR'                        
002000                                             'PL '                        
002100                                             'RC '                        
002110                                             'RCN'                        
002120                                             'RO '                        
002200                                             'RUS'                        
002300                                             'T  '                        
002400                                             'TR '                        
002500                                             'YU '.                       
002600*** END OF VILMAII-COPY LENGTH= 6 BYTES                                   
