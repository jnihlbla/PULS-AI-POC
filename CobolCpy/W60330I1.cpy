000100 01  REQU-W60330I1.                                                       
000200*                                 REQUEST-COPYTEXT PGM W60330             
000300*                                 PARTS HELD AT CUSTOMS                   
000400     03 REQU-IDDC            PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600*                                 WAREHOUSE IDENTIFIER                    
000700     03 REQU-IDARTNR-KEY     PIC 9(9).                                    
000800*                                 ARTIKELNUMMER                           
000900*                                 PART NUMBER                             
001000     03 REQU-IDUSER-003      PIC X(5).                                    
001100*                                 ANSVARIGT USERID INLÄGGN.(R32)          
001200     03 REQU-KVRADER         PIC 9(5).                                    
001300*                                 ANTAL RADER                             
001400*                                 NUMBER OF LINES                         
001500     03 REQU-W60330I1        OCCURS 500 TIMES.                            
001600*                                 REQUEST-COPYTEXT FÖR PGM W60330         
001700*                                 PARTS HELD AT CUSTOMS                   
001800        05 REQU-DAINLEV      PIC 9(16).                                   
001900*                                 INLEVERANS NUMMER                       
002000*                                 CONSIGNMENT IDENTITY                    
002100*                                 (YYYYMMDD+HHMMSSTH)                     
002200        05 REQU-KVANTMOT     PIC 9(6).                                    
002300*                                 ANTAL MOTTAGET                          
002400*                                 QUANTITY RECEIVED                       
002500        05 REQU-IDTRACK      PIC X(25).                                   
002600*                                 TRACKING ID FROM CUSTOMS                
002700*                                 CUSTOMS TRACKING ID                     
002800*** END OF VILMAII-COPY LENGTH= 23521 BYTES                               
