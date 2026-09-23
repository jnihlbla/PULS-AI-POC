000100 01  MOD-W1O16101.                                                        
000200*                                 MOD-COPYTEXT PGM W10161                 
000300*                                 PARTNO PROJECT DCN                      
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDPROJ-IN        PIC X(4).                                    
000900*                                 PARTS PROJEKTIDENTITET                  
001000     03 MOD-IDAO-IN          PIC X(10).                                   
001100*                                 ÄNDRINGSORDERNUMMER                     
001200     03 MOD-IDPROJ-UT        PIC X(4).                                    
001300*                                 PARTS PROJEKTIDENTITET                  
001400     03 MOD-IDAO-UT          PIC X(10).                                   
001500*                                 ÄNDRINGSORDERNUMMER                     
001600     03 MOD-IDARTNR-ENTER    PIC 9(9).                                    
001700*                                 ARTIKELNUMMER                           
001800     03 MOD-IDAO-ENTER       PIC X(10).                                   
001900*                                 ÄNDRINGSORDERNUMMER                     
002000     03 MOD-IDARTNR-PF8      PIC 9(9).                                    
002100*                                 ARTIKELNUMMER                           
002200     03 MOD-IDAO-PF8         PIC X(10).                                   
002300*                                 ÄNDRINGSORDERNUMMER                     
002400     03 MOD-RAD              OCCURS 42 TIMES.                             
002500        05 MOD-AFFECT        PIC X.                                       
002600        05 MOD-IDARTNR       PIC Z(8)9.                                   
002700*                                 ARTIKELNUMMER                           
002800        05 MOD-IDAO          PIC X(10).                                   
002900*                                 ÄNDRINGSORDERNUMMER                     
003000     03 MOD-TEMFSINF         PIC X(61).                                   
003100*                                 INFORMATIONSMEDDELANDE                  
003200*** END COPY W1O16101C0  LENGTH=1011                                      
