000100 01  W33530.                                                              
000200*                                 PAYER. INFORM.  TRANS FROM              
000300*                                 MARKET COMPANY  ARTIKELDISC             
000400     03 IDVTYP               PIC X.                                       
000500*                                 RECORD TYPE VERSION                     
000600     03 IDPROMR.                                                          
000700*                                 PRICE AREA                              
000800        05 IDMARKBO          PIC X.                                       
000900*                                 MARKET COMPANY CODE                     
001000*                                 A = VCS                                 
001100*                                 B = VCEM                                
001200*                                 C = NORDIC WITHOUT SWEDEN               
001300*                                 D = VCUK                                
001400*                                 E = VCNA                                
001500*                                 F = VCI                                 
001600*                                 G = VCAS                                
001700        05 IDPROMRN          PIC X(2).                                    
001800*                                 PRICE AREA SERIALNUMBER                 
001900     03 TISTADAT             PIC S9(7)           COMP-3.                  
002000*                                 GENERAL START DATE                      
002100     03 TISTODAT             PIC S9(7)           COMP-3.                  
002200*                                 GENERAL STOP DATE YYMMDD                
002300     03 IDARTNR              PIC S9(9)           COMP-3.                  
002400*                                 PART NUMBER                             
002500     03 REARTRAB-DO          PIC S9(2)V9(2)      COMP-3.                  
002600*                                 PARTS DISCOUNT DAYORDER                 
002700     03 REARTRAB-BULK        PIC S9(2)V9(2)      COMP-3.                  
002800*                                 PARTS DISCOUNT STOCKORDER               
002900*** END COPY W335301A    LENGTH=23                                        
