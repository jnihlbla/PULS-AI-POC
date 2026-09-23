000100 01  RESP-W90512O1.                                                       
000200*                                 RESPONSE FROM PGM W90512                
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
003000     03 RESP-KVRADER         PIC 9(3).                                    
003100*                                 ANTAL RADER                             
003200     03 RESP-TABSALDO        OCCURS 1 TO 999 TIMES                        
003300                             DEPENDING ON RESP-KVRADER.                   
003400*                                 TABSALDO                                
003500        05 RESP-IDDC         PIC X(2).                                    
003600*                                 IDENTIFIERARE LAGER                     
003700        05 RESP-IDLANDX2     PIC X(2).                                    
003800*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
003900        05 RESP-KVPB-REF     PIC 9(6)V9(1).                               
004000*                                 PERIODBEHOV REFILLING                   
004100        05 RESP-KVOI-RULL-12-CDC                                          
004200                             PIC 9(7).                                    
004300        05 RESP-AARSFORB-CDC PIC 9(7).                                    
004400        05 RESP-KVOI-INNEV   PIC S9(7).                                   
004500*                                 ORDERINGÅNG TILL DC INNEV PER           
004600        05 RESP-OI-PER       OCCURS 6 TIMES.                              
004700           07 RESP-TIPP      PIC 9(2).                                    
004800*                                 PLANERINGSPERIOD (PP)                   
004900*                                 12 PER ÅR                               
005000           07 RESP-TIVV-FOM-TOM                                           
005100                             PIC X(5).                                    
005200           07 RESP-KVOI      PIC S9(7).                                   
005300*                                 ORDERINGÅNG I STYCK PER TIDSENH         
005400*** END OF VILMAII-COPY LENGTH= 116364 BYTES                              
