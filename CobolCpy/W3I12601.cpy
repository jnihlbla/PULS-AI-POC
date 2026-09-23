000100 01  MID-W3I12601.                                                        
000200*                                 MID-COPYTEXT FÖR BILD 3126              
000300*                                 EXCHANGEPRM RE0                         
000400     03 MID-IDUSER-IN        PIC X(8).                                    
000500*                                 ANVÄNDARENS SÄKERHETS ID                
000600     03 MID-IDUSER-UT        PIC X(8).                                    
000700*                                 ANVÄNDARENS SÄKERHETS ID                
000800     03 MID-XPRM-RE0-GRP.                                                 
000900*                                 BYTES POÄNG PRM GRUPP                   
001000        05 MID-FLEXCREP      PIC X.                                       
001100*                                 EXCHANGE REPORT FLAG                    
001200        05 MID-FLEXCBLK      PIC X.                                       
001300*                                 EXCHANGE BLOCK CODE                     
001400        05 MID-REPOINT       PIC X(10).                                   
001500*                                 CONVERSION FACTOR                       
001600        05 MID-TIVV-1        PIC 9(2).                                    
001700*                                 VECKA  (VV)                             
001800        05 MID-TIVV-2        PIC 9(2).                                    
001900*                                 VECKA  (VV)                             
002000        05 MID-TIVV-3        PIC 9(2).                                    
002100*                                 VECKA  (VV)                             
002200        05 MID-TIVV-4        PIC 9(2).                                    
002300*                                 VECKA  (VV)                             
002400        05 MID-IDMAIL        PIC X(60).                                   
002500*                                 MAIL ADRESS                             
002600*** END OF VILMAII-COPY LENGTH= 96 BYTES                                  
