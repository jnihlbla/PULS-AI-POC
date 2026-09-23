000100 01  REQU-WF0251I1.                                                       
000200*                                 REQUEST-COPYTEXT PGM WF0251             
000300*                                 FINANCIAL CUSTOMER LOCATE               
000400     03 REQU-IDLEGSEL-KEY    PIC X(4).                                    
000500*                                 FAKTURERANDE FÖRETAG TEX VCCS           
000600*                                 LEGAL SELLER IDENTITY                   
000700     03 REQU-IDPARTNR-KEY    PIC X(9).                                    
000800*                                 PARTNERNR                               
000900*                                 PARTNER NO                              
001000     03 REQU-IDLANDX3-KEY    PIC X(3).                                    
001100*                                 3-STÄLLIG LANDSBETECKNINGSKOD           
001200*                                 3-LETTER CODE FOR COUNTRY.              
001300     03 REQU-KDPARTTY-KEY    PIC X(3).                                    
001400*                                 TYP AV BETALARE                         
001500*                                 TYPE OF FIN.CUSTOMER                    
001600     03 REQU-KDPARTGR-KEY    PIC X(15).                                   
001700*                                 GRUPP AV BETALARE                       
001800*                                 FIN.CUSTOMER GROUP                      
001900     03 REQU-IDALPHA-KEY     PIC X(10).                                   
002000*                                 ALFANUMERISK SÖKNYCKEL                  
002100*                                 ALPHANUMERICAL SEARCH KEY               
002200     03 REQU-FLPREL-KEY      PIC X.                                       
002300*                                 ALLMÄN FLAGGA                           
002400*                                 GENERAL FLAG                            
002500*** END OF VILMAII-COPY LENGTH= 45 BYTES                                  
