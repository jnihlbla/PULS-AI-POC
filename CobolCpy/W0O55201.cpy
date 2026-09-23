000100 01  MOD-W0O55201.                                                        
000200*                                 MOD-COPYTEXT                            
000300*                                 FÖR W0O55201                            
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600*                                 SCREEN NUMBER                           
000700     03 MOD-TEMFSFEL         PIC X(40).                                   
000800*                                 MFS FELMEDDELANDE                       
000900*                                 MFS ERROR MESSAGE                       
001000     03 MOD-IDDOKTYP-IN      PIC X(8).                                    
001100*                                 DOKUMENTATIONSTYP                       
001200*                                 TYPE OF DOCUMENTATION                   
001300     03 MOD-IDDOKTYP-UT      PIC X(8).                                    
001400*                                 DOKUMENTATIONSTYP                       
001500*                                 TYPE OF DOCUMENTATION                   
001600     03 MOD-IDDOK-IN         PIC X(8).                                    
001700*                                 DOKUMENTATIONSIDENTITET                 
001800*                                 DOCUMENTATION IDENTITY                  
001900     03 MOD-IDDOK-UT         PIC X(8).                                    
002000*                                 DOKUMENTATIONSIDENTITET                 
002100*                                 DOCUMENTATION IDENTITY                  
002200     03 MOD-IDDOKTYP-SPAR    PIC X(8).                                    
002300*                                 DOKUMENTATIONSTYP                       
002400*                                 TYPE OF DOCUMENTATION                   
002500     03 MOD-IDDOK-SPAR       PIC X(8).                                    
002600*                                 DOKUMENTATIONSIDENTITET                 
002700*                                 DOCUMENTATION IDENTITY                  
002800     03 MOD-IDDOKTYP-ENTER   PIC X(8).                                    
002900*                                 DOKUMENTATIONSTYP                       
003000*                                 TYPE OF DOCUMENTATION                   
003100     03 MOD-IDDOK-ENTER      PIC X(8).                                    
003200*                                 DOKUMENTATIONSIDENTITET                 
003300*                                 DOCUMENTATION IDENTITY                  
003400     03 MOD-RAD              OCCURS 14 TIMES.                             
003500        05 MOD-IDDOKTYP      PIC X(8).                                    
003600*                                 DOKUMENTATIONSTYP                       
003700*                                 TYPE OF DOCUMENTATION                   
003800        05 MOD-IDDOK         PIC X(8).                                    
003900*                                 DOKUMENTATIONSIDENTITET                 
004000*                                 DOCUMENTATION IDENTITY                  
004100        05 MOD-IDUSER        PIC X(8).                                    
004200*                                 ANVÄNDARENS SÄKERHETS ID                
004300*                                 USER SECURITY-IDENTITY                  
004400        05 MOD-TIREGDAT      PIC 9(6).                                    
004500*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
004600*                                 REGISTRATION DATE (YYMMDD)              
004700        05 MOD-TIREGTID      PIC 9(6).                                    
004800*                                 REGISTRERINGSTID                        
004900*                                 GENERAL REGISTRATION TIME               
005000        05 MOD-BEDOK         PIC X(25).                                   
005100*                                 DOKUMENTATIONSBESKRIVNING               
005200*                                 DOCUMENTATION DESCRIPTION               
005300     03 MOD-TEMFSINF         PIC X(55).                                   
005400*                                 INFORMATIONSMEDDELANDE                  
005500*                                 INFORMATION MESSAGE                     
005600*** END OF VILMAII-COPY LENGTH= 1017 BYTES                                
