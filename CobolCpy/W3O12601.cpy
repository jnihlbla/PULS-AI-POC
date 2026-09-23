000100 01  MOD-W3O12601.                                                        
000200*                                 MOD-COPYTEXT FÖR BILD 3126              
000300*                                 BYTES POÄNG PRM                         
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDUSER-IN        PIC X(8).                                    
000900*                                 ANVÄNDARENS SÄKERHETS ID                
001000     03 MOD-IDUSER-UT        PIC X(8).                                    
001100*                                 ANVÄNDARENS SÄKERHETS ID                
001200     03 MOD-DATA-IN.                                                      
001300*                                 RAPPORTERINGS-FÄLT                      
001400        05 MOD-FLEXCREP-ATTR PIC X(2).                                    
001500*                                 MFS ATTRIBUTFÄLT                        
001600        05 MOD-FLEXCREP      PIC X.                                       
001700*                                 EXCHANGE REPORT FLAG                    
001800        05 MOD-FLEXCBLK-ATTR PIC X(2).                                    
001900*                                 MFS ATTRIBUTFÄLT                        
002000        05 MOD-FLEXCBLK      PIC X.                                       
002100*                                 EXCHANGE BLOCK CODE                     
002200        05 MOD-REPOINT-ATTR  PIC X(2).                                    
002300*                                 MFS ATTRIBUTFÄLT                        
002400        05 MOD-REPOINT       PIC Z(4)9.9(4).                              
002500*                                 CONVERSION FACTOR                       
002600        05 MOD-TIVV-1-ATTR   PIC X(2).                                    
002700*                                 MFS ATTRIBUTFÄLT                        
002800        05 MOD-TIVV-1        PIC Z(2).                                    
002900*                                 VECKA  (VV)                             
003000        05 MOD-TIVV-2-ATTR   PIC X(2).                                    
003100*                                 MFS ATTRIBUTFÄLT                        
003200        05 MOD-TIVV-2        PIC Z(2).                                    
003300*                                 VECKA  (VV)                             
003400        05 MOD-TIVV-3-ATTR   PIC X(2).                                    
003500*                                 MFS ATTRIBUTFÄLT                        
003600        05 MOD-TIVV-3        PIC Z(2).                                    
003700*                                 VECKA  (VV)                             
003800        05 MOD-TIVV-4-ATTR   PIC X(2).                                    
003900*                                 MFS ATTRIBUTFÄLT                        
004000        05 MOD-TIVV-4        PIC Z(2).                                    
004100*                                 VECKA  (VV)                             
004200        05 MOD-IDMAIL-ATTR   PIC X(2).                                    
004300*                                 MFS ATTRIBUTFÄLT                        
004400        05 MOD-IDMAIL        PIC X(60).                                   
004500*                                 MAIL ADRESS                             
004600     03 MOD-TEMFSINF         PIC X(55).                                   
004700*                                 INFORMATIONSMEDDELANDE                  
004800*** END OF VILMAII-COPY LENGTH= 211 BYTES                                 
