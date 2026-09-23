000100 01  RESP-WF0251O1.                                                       
000200*                                 RESPONS-COPYTEXT FÖR PGM WF0251         
000300*                                 FINANCIAL CUSTOMER LOCATE               
000400     03 RESP-IDLEGSEL-KEY    PIC X(4).                                    
000500*                                 FAKTURERANDE FÖRETAG TEX VCCS           
000600*                                 LEGAL SELLER IDENTITY                   
000700     03 RESP-IDPARTNR-KEY    PIC X(9).                                    
000800*                                 PARTNERNR                               
000900*                                 PARTNER NO                              
001000     03 RESP-IDLANDX3-KEY    PIC X(3).                                    
001100*                                 3-STÄLLIG LANDSBETECKNINGSKOD           
001200*                                 3-LETTER CODE FOR COUNTRY.              
001300     03 RESP-KDPARTTY-KEY    PIC X(3).                                    
001400*                                 TYP AV BETALARE                         
001500*                                 TYPE OF FIN.CUSTOMER                    
001600     03 RESP-KDPARTGR-KEY    PIC X(15).                                   
001700*                                 GRUPP AV BETALARE                       
001800*                                 FIN.CUSTOMER GROUP                      
001900     03 RESP-IDALPHA-KEY     PIC X(10).                                   
002000*                                 ALFANUMERISK SÖKNYCKEL                  
002100*                                 ALPHANUMERICAL SEARCH KEY               
002200     03 RESP-FLPREL-KEY      PIC X.                                       
002300*                                 ALLMÄN FLAGGA                           
002400*                                 GENERAL FLAG                            
002500     03 RESP-BELEGRAD-1      PIC X(35).                                   
002600*                                 DEL AV LEGAL SELLER NAMN                
002700*                                 PART OF LEGAL SELLER NAME               
002800     03 RESP-KVRADER         PIC Z(4)9.                                   
002900*                                 ANTAL RADER                             
003000*                                 NUMBER OF LINES                         
003100     03 RESP-TABELLRAD       OCCURS 500 TIMES.                            
003200*                                 GRUPP MED TABELLRADER                   
003300        05 RESP-IDPARTNR-LINE                                             
003400                             PIC X(9).                                    
003500*                                 PARTNERNR                               
003600*                                 PARTNER NO                              
003700        05 RESP-BEBET-NAME1-LINE                                          
003800                             PIC X(35).                                   
003900*                                 DEL AV BETALNINGSANSVARIGS NAMN         
004000*                                 PART OF FINANCIAL CUSTOMER NAME         
004100        05 RESP-ADBET-STREET-LINE                                         
004200                             PIC X(35).                                   
004300*                                 BETALARENS GATUADRESS                   
004400*                                 PAYER ADDRESS STREET                    
004500        05 RESP-ADBET-BOX-LINE                                            
004600                             PIC X(10).                                   
004700*                                 BOXADRESS BETALNINGSANSVARIG            
004800*                                 PAYER BOX ADDRESS                       
004900        05 RESP-ADBET-PCODE-LINE                                          
005000                             PIC X(10).                                   
005100*                                 BETALARENS STADSADRESS POSTNR           
005200*                                 PAYER ADDRESS POSTAL CODE               
005300        05 RESP-ADBET-CITY-LINE                                           
005400                             PIC X(35).                                   
005500*                                 BETALARENS STADSADRESS                  
005600*                                 PAYER ADDRESS CITY                      
005700        05 RESP-IDLANDX3-LINE                                             
005800                             PIC X(3).                                    
005900*                                 3-STÄLLIG LANDSBETECKNINGSKOD           
006000*                                 3-LETTER CODE FOR COUNTRY.              
006100        05 RESP-FLCOMING-LINE                                             
006200                             PIC X.                                       
006300*                                 ALLMÄN FLAGGA                           
006400*                                 GENERAL FLAG                            
006500*** END OF VILMAII-COPY LENGTH= 69085 BYTES                               
