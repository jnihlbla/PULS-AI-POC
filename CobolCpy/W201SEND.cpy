000100 01  SEND-W201SEND.                                                       
000200*                                 LÄNKAREA TILL W201SEND  -               
000300     03 SEND-IDMAIL          PIC X(60).                                   
000400*                                 MAIL ADRESS                             
000500     03 SEND-W2I10902.                                                    
000600*                                 MID FRÅN ANDRA PROGRAM                  
000700        05 SEND-KVANTART     PIC 9(5).                                    
000800*                                 ANTAL-ARTIKLAR                          
000900        05 SEND-INAREA       OCCURS 23 TIMES.                             
001000*                                 ORDERINGÅNG  INPUT                      
001100           07 SEND-IDARTNR   PIC 9(8).                                    
001200*                                 ARTIKELNUMMER                           
001300           07 SEND-IDDC      PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500           07 SEND-KDTECKEN  PIC X.                                       
001600*                                 PLUS ELLER MINUS (+ -)                  
001700           07 SEND-KDOI      PIC X(2).                                    
001800*                                 ORDERINGÅNGSTYP                         
001900           07 SEND-CLEARGROUP.                                            
002000*                                 CLEARINGAREA FÖR ORDERINGÅNG            
002100              09 SEND-CLEARAREA                                           
002200                             OCCURS 7 TIMES.                              
002300*                                 CLEARINGAREA FÖR ORDERINGÅNG            
002400                 11 SEND-IDDC-CLEAR                                       
002500                             PIC X(2).                                    
002600*                                 LAGERPRIORITERING VID                   
002700*                                 ORDERCLEARING                           
002800                 11 SEND-FLLF                                             
002900                             PIC X.                                       
003000*                                 ARTIKEL LAGERFÖRES                      
003100                 11 SEND-FLCLEAR                                          
003200                             PIC X.                                       
003300*                                 ORDERRAD CLEAR FLAGGA                   
003400           07 SEND-KVOI      PIC 9(7).                                    
003500*                                 ORDERINGÅNG I STYCK PER TIDSENH         
003600           07 SEND-TIUPPDAT  PIC 9(6).                                    
003700*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
003800     03 SEND-TEXT            PIC X(80).                                   
003900*** END OF VILMAII-COPY LENGTH= 1387 BYTES                                
