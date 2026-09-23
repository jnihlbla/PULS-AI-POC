000100 01  W330381A.                                                            
000200     03 IDPTYP               PIC X(3).                                    
000300*                                 RECORD TYPE                             
000400     03 IDVTYP               PIC X.                                       
000500*                                 RECORD TYPE VERSION                     
000600     03 IDARTNR              PIC S9(9)           COMP-3.                  
000700*                                 PART NUMBER                             
000800     03 IDPROMR.                                                          
000900*                                 PRICE AREA                              
001000        05 IDMARKBO          PIC X.                                       
001100*                                 MARKET COMPANY CODE                     
001200*                                 A = VCS                                 
001300*                                 B = VCEM                                
001400*                                 C = NORDIC WITHOUT SWEDEN               
001500*                                 D = VCUK                                
001600*                                 E = VCNA                                
001700*                                 F = VCI                                 
001800*                                 G = VCAS                                
001900        05 IDPROMRN          PIC X(2).                                    
002000*                                 PRICE AREA SERIALNUMBER                 
002100     03 SUARTFSG-DO-RAAR     PIC S9(9)V9(2)      COMP-3.                  
002200*                                 SALES AMOUNT CURRENT ROLLING            
002300*                                 YEAR. FOR DAY ORDER                     
002400     03 SULEVANT-DO-RAAR     PIC S9(9)           COMP-3.                  
002500*                                 SUM DELIVERED ART ROLLING YEAR          
002600*                                 DAY ORDER                               
002700     03 SUARTFSG-RAAR        PIC S9(9)V9(2)      COMP-3.                  
002800*                                 SALES AMOUNT CURRENT ROLLING            
002900*                                 YEAR                                    
003000     03 SULEVANT-RAAR        PIC S9(9)           COMP-3.                  
003100*                                 SUM DELIVERED ART ROLLING YEAR          
003200*                                                                         
003300*** END COPY W330381A    LENGTH=34                                        
