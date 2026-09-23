000100 01  W33577.                                                              
000200*                                 HSSR ON WDB6                            
000300     03 IDDISTR              PIC 9(4).                                    
000400*                                 DISTRICT NUMBER                         
000500     03 IDPROMR.                                                          
000600*                                 PRICE AREA                              
000700        05 IDMARKBO          PIC X.                                       
000800*                                 MARKET COMPANY CODE                     
000900*                                 A = VCS                                 
001000*                                 B = VCEM                                
001100*                                 C = NORDIC WITHOUT SWEDEN               
001200*                                 D = VCUK                                
001300*                                 E = VCNA                                
001400*                                 F = VCI                                 
001500*                                 G = VCAS                                
001600        05 IDPROMRN          PIC X(2).                                    
001700*                                 PRICE AREA SERIALNUMBER                 
001800     03 IDPARTNR             PIC X(9).                                    
001900*                                 PARTNER NO                              
002000     03 BEBETRAD             PIC X(35).                                   
002100*                                 PART OF FINANCIAL CUSTOMER NAME         
002200     03 FLOKFAK-G            PIC X.                                       
002300*                                 INVOICETYPE G ALLOWED                   
002400     03 FLOKFAK-K            PIC X.                                       
002500*                                 INVOICETYPE K ALLOWED                   
002600     03 FLOKFAK-N            PIC X.                                       
002700*                                 INVOICETYPE N ALLOWED                   
002800     03 FLOKFAK-R            PIC X.                                       
002900*                                 INVOICETYPE R ALLOWED                   
003000     03 KDVALISO             PIC X(3).                                    
003100*                                 CURRENCY CODE BY ISO-STANDARD.          
003200     03 REEMBHNT             PIC 9(2)V9(1).                               
003300*                                 PACKING AND HANDLING (%)                
003400     03 KDKREDSP             PIC X.                                       
003500*                                 CREDIT STOP FINANCIAL CUSTOMER          
003600*** END OF VILMAII-COPY LENGTH= 62 BYTES                                  
