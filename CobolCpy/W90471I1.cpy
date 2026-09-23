000100 01  REQU-W90471I1.                                                       
000200*                                 REQUEST-COPYTEXT PGM W90471             
000300*                                 SPIE - PROJEKTUPPFÖLJN PÅ ÄO            
000400     03 REQU-IDAO-NEXT       PIC X(10).                                   
000500*                                 ÄNDRINGSORDERNUMMER                     
000600     03 REQU-IDARTNR-NEXT    PIC 9(8).                                    
000700*                                 ARTIKELNUMMER                           
000800     03 REQU-KVRADER-MAX     PIC 9(3).                                    
000900*                                 MAX INDEX KOPPLAT TILL OCCURS N         
001000*                                 EDAN.                                   
001100     03 REQU-IDAO            OCCURS 1 TO 200 TIMES                        
001200                             DEPENDING ON REQU-KVRADER-MAX                
001300                             PIC X(10).                                   
001400*                                 ÄNDRINGSORDERNUMMER                     
001500*** END OF VILMAII-COPY LENGTH= 2021 BYTES                                
