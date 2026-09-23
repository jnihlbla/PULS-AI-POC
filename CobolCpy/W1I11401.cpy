000100 01  MID-W1I11401.                                                        
000200*                                 MID-COPYTEXT PGM W10114                 
000300*                                 PARTS   REGISTRATION                    
000400     03 MID-IDARTNR-IN       PIC X(9).                                    
000500*                                 ARTIKELNUMMER                           
000600     03 MID-IDARTNR-UT       PIC X(9).                                    
000700*                                 ARTIKELNUMMER                           
000800     03 MID-KDBASLM-ENTER    PIC X(6).                                    
000900*                                 BASLAGERMARKNAD                         
001000     03 MID-KDBASLM-PF8      PIC X(6).                                    
001100*                                 BASLAGERMARKNAD                         
001200     03 MID-IDAO-KEY         PIC X(10).                                   
001300*                                 ÄNDRINGSORDERNUMMER                     
001400     03 MID-IDPROJ-KEY       PIC X(4).                                    
001500*                                 PARTS PROJEKTIDENTITET                  
001600     03 MID-IDARTNR-NEW      PIC X(9).                                    
001700*                                 ARTIKELNUMMER                           
001800     03 MID-FLBASL           PIC X.                                       
001900*                                 BASLAGER MARKNADSKÖ FLAGGA              
002000     03 MID-FLATERPAKOE      PIC X.                                       
002100     03 MID-RAD              OCCURS 9 TIMES.                              
002200        05 MID-COL           OCCURS 4 TIMES.                              
002300           07 MID-KDBASLM    PIC X(6).                                    
002400*                                 BASLAGERMARKNAD                         
002500           07 MID-AFFECT     PIC X.                                       
002600     03 MID-TEARTNOT-VAR     PIC X(40).                                   
002700*                                 ARTIKEL NOTERING                        
002800     03 MID-TEARTNOT-BASL    PIC X(40).                                   
002900*                                 ARTIKEL NOTERING                        
003000     03 MID-IDARTNR-MOTSV    PIC X(9).                                    
003100*                                 ARTIKELNUMMER                           
003200*** END COPY W1I11401C0  LENGTH=396                                       
