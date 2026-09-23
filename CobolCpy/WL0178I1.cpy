000100 01  REQU-WL0178I1.                                                       
000200*                                 REQUEST TO PGM WL0178                   
000300     03 REQU-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 REQU-IDLISTNR-KEY    PIC 9(6).                                    
000600*                                 LISTNUMMER                              
000700     03 REQU-IDUSER-KEY      PIC X(8).                                    
000800*                                 ANVÄNDARENS SÄKERHETS ID                
000900     03 REQU-FLDIFF          PIC X.                                       
001000*                                 JA/NEJ-FLAGGA                           
001100     03 REQU-KVRADER         PIC 9(5).                                    
001200*                                 ANTAL RADER                             
001300     03 REQU-INPUT-GRP.                                                   
001400        05 REQU-INPUT        OCCURS 10 TIMES.                             
001500           07 REQU-IDARTNR   PIC 9(8).                                    
001600*                                 ARTIKELNUMMER                           
001700           07 REQU-BEART-SVE PIC X(25).                                   
001800           07 REQU-KVAKS     PIC 9(7).                                    
001900*                                 ANKOMSTSALDO                            
002000           07 REQU-KVLS      PIC X(7).                                    
002100*                                 LAGERSALDO                              
002200           07 REQU-KVEFRS    PIC 9(7).                                    
002300*                                 EJ FAKTURERAT ANTAL STYCK               
002400           07 REQU-ANTAL-IN  PIC 9(6).                                    
002500*                                 ANTAL                                   
002600           07 REQU-TECKEN-IN PIC X.                                       
002700           07 REQU-DIFFERANS-IN                                           
002800                             PIC 9(6).                                    
002900*                                 ANTAL                                   
003000           07 REQU-FLOMINV-IN                                             
003100                             PIC X.                                       
003200           07 REQU-IDMSG-ERROR-LINE                                       
003300                             PIC X(3).                                    
003400*                                 FELMEDDELANDE ID                        
003500     03 REQU-IN-ONE-FILLER REDEFINES REQU-INPUT-GRP.                      
003600        05 REQU-IN-ONE.                                                   
003700           07 REQU-IDARTNR-ONE                                            
003800                             PIC 9(8).                                    
003900*                                 ARTIKELNUMMER                           
004000           07 REQU-BEART-SVE-ONE                                          
004100                             PIC X(25).                                   
004200           07 REQU-KVAKS-ONE PIC 9(7).                                    
004300*                                 ANKOMSTSALDO                            
004400           07 REQU-KVLS-ONE  PIC X(7).                                    
004500*                                 LAGERSALDO                              
004600           07 REQU-KVEFRS-ONE                                             
004700                             PIC 9(7).                                    
004800*                                 EJ FAKTURERAT ANTAL STYCK               
004900           07 REQU-ANTAL-IN-ONE                                           
005000                             PIC 9(6).                                    
005100*                                 ANTAL                                   
005200           07 REQU-TECKEN-IN-ONE                                          
005300                             PIC X.                                       
005400           07 REQU-DIFFERANS-IN-ONE                                       
005500                             PIC 9(6).                                    
005600*                                 ANTAL                                   
005700           07 REQU-FLOMINV-IN-ONE                                         
005800                             PIC X.                                       
005900           07 REQU-IDMSG-ERROR-LINE-ONE                                   
006000                             PIC X(3).                                    
006100*                                 FELMEDDELANDE ID                        
006200           07 REQU-FILLER    PIC X(630).                                  
006300        05 FILLER            PIC X(9).                                    
006400*** END OF VILMAII-COPY LENGTH= 732 BYTES                                 
