000100 01  MID-W1I16101.                                                        
000200*                                 MID-COPYTEXT PGM W10161                 
000300*                                 PARTNO PROJECT DCN                      
000400     03 MID-IDPROJ-IN        PIC X(4).                                    
000500*                                 PARTS PROJEKTIDENTITET                  
000600     03 MID-IDAO-IN          PIC X(10).                                   
000700*                                 ÄNDRINGSORDERNUMMER                     
000800     03 MID-IDPROJ-UT        PIC X(4).                                    
000900*                                 PARTS PROJEKTIDENTITET                  
001000     03 MID-IDAO-UT          PIC X(10).                                   
001100*                                 ÄNDRINGSORDERNUMMER                     
001200     03 MID-IDARTNR-ENTER    PIC 9(9).                                    
001300*                                 ARTIKELNUMMER                           
001400     03 MID-IDAO-ENTER       PIC X(10).                                   
001500*                                 ÄNDRINGSORDERNUMMER                     
001600     03 MID-IDARTNR-PF8      PIC 9(9).                                    
001700*                                 ARTIKELNUMMER                           
001800     03 MID-IDAO-PF8         PIC X(10).                                   
001900*                                 ÄNDRINGSORDERNUMMER                     
002000     03 MID-RAD              OCCURS 42 TIMES.                             
002100        05 MID-AFFECT        PIC X.                                       
002200        05 MID-IDARTNR       PIC X(9).                                    
002300*                                 ARTIKELNUMMER                           
002400*** END COPY W1I16101C0  LENGTH=486                                       
