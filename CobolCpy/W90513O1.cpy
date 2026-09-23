000100 01  RESP-W90513O1.                                                       
000200*                                 RESPONSE FROM PGM W90513                
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
003000     03 RESP-W200PINF.                                                    
003100*                                 DATA FROM SUB PROGRAM W200PINF          
003200        05 RESP-BEART-ENG    PIC X(25).                                   
003300*                                 ENGELSK ARTIKELBENÄMNING                
003400        05 RESP-KDERS        PIC 9(2).                                    
003500*                                 ERSÄTTNINGSKOD                          
003600        05 RESP-KDAVT        PIC 9.                                       
003700*                                 AVTALSMÄRKNING                          
003800        05 RESP-IDANSK       PIC 9(3).                                    
003900*                                 ANSKAFFARNUMMER                         
004000        05 RESP-IDNAMN-ANSK  PIC X(40).                                   
004100*                                 NAMN                                    
004200        05 RESP-KDFARLIG     PIC 9.                                       
004300*                                 KOD FÖR FARLIGT GODS                    
004400        05 RESP-IDLEVNR-MFG  PIC X(5).                                    
004500*                                 LEVERANTÖRNUMMER                        
004600        05 RESP-IDLEVNR-SHIP PIC X(5).                                    
004700*                                 SKEPPANDE LEVERANTÖR                    
004800        05 RESP-KVRADER      PIC 9(3).                                    
004900*                                 ANTAL RADER                             
005000        05 RESP-KIT-DATA     OCCURS 1 TO 999 TIMES                        
005100                             DEPENDING ON RESP-KVRADER.                   
005200           07 RESP-IDARTNR-SATS                                           
005300                             PIC 9(9).                                    
005400*                                 ARTIKELNUMMER FÖR SATS                  
005500           07 RESP-REANTPSA  PIC 9V9(3).                                  
005600*                                 ANTAL PER SATS                          
005700*** END OF VILMAII-COPY LENGTH= 13549 BYTES                               
