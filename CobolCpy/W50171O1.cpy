000100 01  RESP-W50171O1.                                                       
000200*                                 RESPONSE FROM PGM W50171                
000300*                                                                         
000400     03 RESP-WZ01RES2.                                                    
000500*                                 THE FIRST FIELDS IN AN RESPONSE         
000600*                                 SENT AS A RESULT OF A REQUEST           
000700*                                 FROM ONE SYSTEM COMPONENT TO            
000800*                                 ANOTHER.                                
000900*                                 !!! SECOND VERSION / IDRESVER =         
001000*                                  002 !!!                                
001100        05 RESP-IDRESVER     PIC 9(3).                                    
001200*                                 VERSIONSNUMMER FÖR MEDDELANDE O         
001300*                                 M SVARSHUVUD                            
001400        05 RESP-IDMSG-INFO   PIC X(3).                                    
001500*                                 INFORMATIONSMEDDELANDE ID               
001600        05 RESP-IDMSG-ERROR  PIC X(3).                                    
001700*                                 FELMEDDELANDE ID                        
001800        05 RESP-IDELMT-ERROR PIC X(16).                                   
001900*                                 DATAELEMENTIDENTITET                    
002000        05 RESP-KDSTATUS-API PIC 9(3).                                    
002100        05 RESP-MESSAGES     OCCURS 2 TIMES.                              
002200*                                                                         
002300           07 RESP-IDMSG     PIC X(10).                                   
002400*                                 MEDDELANDE NUMMER                       
002500           07 RESP-MESSAGE   PIC X(100).                                  
002600*                                 MEDDELANDE                              
002700        05 FILLER            PIC X(220).                                  
002800     03 RESP-IDARTNR-UT      PIC 9(9).                                    
002900*                                 ARTIKELNUMMER                           
003000     03 RESP-PRARTSTD-UT     PIC S9(7)V9(2).                              
003100*                                 ARTIKELSTANDARDPRIS                     
003200     03 RESP-KDVALISO-UT     PIC X(3).                                    
003300*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
003400*** END OF VILMAII-COPY LENGTH= 489 BYTES                                 
