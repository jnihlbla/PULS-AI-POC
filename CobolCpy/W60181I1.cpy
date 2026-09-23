000100 01  REQU-W60181I1.                                                       
000200*                                 COPYTEXT FÖR REQU W6018100              
000300*                                                                         
000400     03 REQU-IDARTNR-KEY     PIC X(9).                                    
000500*                                 ARTIKELNUMMER                           
000600     03 REQU-IDSPRAK         PIC X(2).                                    
000700*                                 2-STÄLLIG ISO SPRÅKKOD                  
000800     03 REQU-IDDC-KEY        PIC X(2).                                    
000900*                                 IDENTIFIERARE LAGER                     
001000     03 REQU-IDDC            PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200     03 REQU-BEFT            PIC 9(2).                                    
001300*                                 FÖRPACKNINGSTYP                         
001400     03 REQU-KDFORP.                                                      
001500*                                 FÖRPACKNINGSKOD                         
001600        05 REQU-KDFORPPL     PIC 9.                                       
001700*                                 FÖRPACKNINGSPLATS                       
001800        05 REQU-KDFORPGP     PIC 9(2).                                    
001900*                                 FÖRPACKNINGSGRUPP                       
002000        05 REQU-KDFORPUF     PIC 9.                                       
002100*                                 UPPRÄKNINGSFAKTOR                       
002200     03 REQU-IDUSER-IN       PIC X(8).                                    
002300*                                 ANVÄNDARENS SÄKERHETS ID                
002400     03 REQU-TEBEFT          PIC X(40).                                   
002500*                                 TEXT FÖRPACKNINGSINSTRUKTION            
002600     03 REQU-TEBEFT-79       PIC X(79).                                   
002700     03 REQU-TEBEFT02-79     PIC X(79).                                   
002800     03 REQU-KVRADER         PIC 9(5).                                    
002900*                                 ANTAL RADER                             
003000     03 REQU-DAREGDAT-START  PIC 9(8).                                    
003100*                                 DATUMETS 9-KOMPLEMENT                   
003200     03 REQU-TIKLOCK-START   PIC 9(9).                                    
003300*                                 TID LAGRAT SOM 9-KOMPLEMENT             
003400*** END OF VILMAII-COPY LENGTH= 249 BYTES                                 
