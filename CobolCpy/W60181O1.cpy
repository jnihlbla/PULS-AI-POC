000100 01  RESP-W60181O1.                                                       
000200*                                 COPYTEXT FOR RESP W60181O1              
000300*                                 FÖRPACKNINGSTYP                         
000400     03 RESP-IDDC-KEY        PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600*                                 WAREHOUSE IDENTIFIER                    
000700     03 RESP-BEART-UT        PIC X(25).                                   
000800*                                 ARTIKELBENÄMNING                        
000900*                                 PART DESCRIPTION                        
001000     03 RESP-BEFT-UT         PIC Z9.                                      
001100*                                 FÖRPACKNINGSTYP                         
001200*                                 PACKAGING TYPE                          
001300     03 RESP-KDFORP-UT.                                                   
001400*                                 FÖRPACKNINGSKOD                         
001500*                                 PACKAGING CODE                          
001600        05 RESP-KDFORPPL     PIC 9.                                       
001700*                                 FÖRPACKNINGSPLATS                       
001800*                                 PREPACKING PLACE                        
001900        05 RESP-KDFORPGP     PIC 9(2).                                    
002000*                                 FÖRPACKNINGSGRUPP                       
002100*                                 PREPACKING GROUP                        
002200        05 RESP-KDFORPUF     PIC 9.                                       
002300*                                 UPPRÄKNINGSFAKTOR                       
002400*                                 ENUMERATION                             
002500     03 RESP-IDUSER-UT       PIC X(8).                                    
002600*                                 ANVÄNDARENS SÄKERHETS ID                
002700*                                 USER SECURITY-IDENTITY                  
002800     03 RESP-TEBEFT-UT       PIC X(40).                                   
002900*                                 TEXT FÖRPACKNINGSINSTRUKTION            
003000     03 RESP-TEBEFT-79-UT    PIC X(79).                                   
003100     03 RESP-TEBEFT02-79-UT  PIC X(79).                                   
003200     03 RESP-TIREGDAT-UT     PIC 9(6).                                    
003300*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
003400*                                 REGISTRATION DATE (YYMMDD)              
003500     03 RESP-BEFT-ATTR       PIC X(2).                                    
003600*                                 MFS ATTRIBUTFÄLT                        
003700     03 RESP-BEFT            PIC Z9.                                      
003800*                                 FÖRPACKNINGSTYP                         
003900*                                 PACKAGING TYPE                          
004000     03 RESP-KDFORP-ATTR     PIC X(2).                                    
004100*                                 MFS ATTRIBUTFÄLT                        
004200     03 RESP-KDFORP.                                                      
004300*                                 FÖRPACKNINGSKOD                         
004400*                                 PACKAGING CODE                          
004500        05 RESP-KDFORPPL     PIC 9.                                       
004600*                                 FÖRPACKNINGSPLATS                       
004700*                                 PREPACKING PLACE                        
004800        05 RESP-KDFORPGP     PIC 9(2).                                    
004900*                                 FÖRPACKNINGSGRUPP                       
005000*                                 PREPACKING GROUP                        
005100        05 RESP-KDFORPUF     PIC 9.                                       
005200*                                 UPPRÄKNINGSFAKTOR                       
005300*                                 ENUMERATION                             
005400     03 RESP-TEBEFT-ATTR     PIC X(2).                                    
005500*                                 MFS ATTRIBUTFÄLT                        
005600     03 RESP-TEBEFT          PIC X(40).                                   
005700*                                 TEXT FÖRPACKNINGSINSTRUKTION            
005800     03 RESP-TEBEFT-79-ATTR  PIC X(2).                                    
005900*                                 MFS ATTRIBUTFÄLT                        
006000     03 RESP-TEBEFT-79       PIC X(79).                                   
006100     03 RESP-TEBEFT02-79-ATTR                                             
006200                             PIC X(2).                                    
006300*                                 MFS ATTRIBUTFÄLT                        
006400     03 RESP-TEBEFT02-79     PIC X(79).                                   
006500     03 RESP-DAREGDAT-NEXT   PIC 9(8).                                    
006600*                                 DATUMETS 9-KOMPLEMENT                   
006700*                                 DATES 9-COMPLEMENT                      
006800     03 RESP-TIKLOCK-NEXT    PIC 9(9).                                    
006900*                                 TID LAGRAT SOM 9-KOMPLEMENT             
007000*                                 TIME SAVED AS 9-COMPLEMENT              
007100     03 RESP-KVRADER         PIC 9(5).                                    
007200*                                 ANTAL RADER                             
007300*                                 NUMBER OF LINES                         
007400     03 RESP-LINE            OCCURS 500 TIMES.                            
007500*                                 RADER SOM VISAR HISTORIK PÅ             
007600*                                 FÖRPACKNINGSINSTRUKTIONER               
007700        05 RESP-BEFT-HIST-LINE                                            
007800                             PIC Z9.                                      
007900*                                 FÖRPACKNINGSTYP                         
008000*                                 PACKAGING TYPE                          
008100        05 RESP-KDFORP-HIST-LINE.                                         
008200*                                 FÖRPACKNINGSKOD                         
008300*                                 PACKAGING CODE                          
008400           07 RESP-KDFORPPL  PIC 9.                                       
008500*                                 FÖRPACKNINGSPLATS                       
008600*                                 PREPACKING PLACE                        
008700           07 RESP-KDFORPGP  PIC 9(2).                                    
008800*                                 FÖRPACKNINGSGRUPP                       
008900*                                 PREPACKING GROUP                        
009000           07 RESP-KDFORPUF  PIC 9.                                       
009100*                                 UPPRÄKNINGSFAKTOR                       
009200*                                 ENUMERATION                             
009300        05 RESP-IDUSER-HIST-LINE                                          
009400                             PIC X(8).                                    
009500*                                 ANVÄNDARENS SÄKERHETS ID                
009600*                                 USER SECURITY-IDENTITY                  
009700        05 RESP-TIREGDAT-HIST-LINE                                        
009800                             PIC X(6).                                    
009900*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
010000*                                 REGISTRATION DATE (YYMMDD)              
010100        05 RESP-TEBEFT-HIST-LINE                                          
010200                             PIC X(40).                                   
010300*                                 TEXT FÖRPACKNINGSINSTRUKTION            
010400        05 RESP-TEBEFT-79-HIST-LINE                                       
010500                             PIC X(79).                                   
010600        05 RESP-TEBEFT02-79-HIST-LINE                                     
010700                             PIC X(79).                                   
010800*** END OF VILMAII-COPY LENGTH= 109481 BYTES                              
